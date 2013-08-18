defmodule Debugger.Runner do
  alias Debugger.Coordinator
  alias Debugger.Evaluator
  alias Debugger.PIDTable
  alias Debugger.Runner
  
  import Debugger.Escape

  # functions manipulating state coming from Coordinator
  def change_state(fun) do
    coord = PIDTable.get(self)
    state = Coordinator.get_state(coord)

    case fun.(state) do
      { :exception, kind, reason, stacktrace } ->
        { :exception, kind, reason, stacktrace }
      { status, result, new_state } ->
        Coordinator.put_state(coord, new_state)
        { status, result }
    end
  end

  def eval_change_state(expr) do
    change_state &Evaluator.eval_quoted(expr, &1)
  end

  def with_state(fun) do
    coord = PIDTable.get(self)
    state = Coordinator.get_state(coord)

    case fun.(state) do
      { :exception, kind, reason, stacktrace } ->
        { :exception, kind, reason, stacktrace }
      { status, result, _state } ->
        { status, result }
      { status, result } ->
        { status, result }
    end
  end

  def eval_with_state(expr) do
    with_state &Evaluator.eval_quoted(expr, &1)
  end

  # run fun on a state to be discarded
  def do_and_discard_state(fun) do
    coord = PIDTable.get(self)
    Coordinator.push_stack(coord)
    result = fun.()
    Coordinator.pop_stack(coord)

    result
  end

  # expansions that lead to case-like expressions should be kept
  defp do_or_expand(expr, fun) do 
    expanded = with_state &Evaluator.expand(expr, &1)

    case expanded do
      { :case, _, _ } ->
        next(expanded)
      _ ->
        fun.()
    end
  end

  # maps fun |> filter over col while fun |> condition is true
  # otherwise returns fun(failing_element)
  def filter_map_while(col, condition, filter, fun) do 
    ret = do_filter_map_while(col, condition, filter, fun, [])
    case ret do
      list when is_list(list) ->
        Enum.reverse list
      other ->
        other
    end
  end

  defp do_filter_map_while([], _con, _fil, _fun, acc), do: acc
  defp do_filter_map_while([h | t], con, fil, fun, acc) do
    fh = fun.(h)
    if con.(fh) do
      do_filter_map_while(t, con, fil, fun, [fil.(fh) | acc])
    else
      fh
    end
  end

  # maps next/1 while status returned is :ok, otherwise returns the
  # failing element of the list with its status
  def map_next_while_ok(expr_list) do
    v = filter_map_while expr_list, &is_status_ok?(&1), &strip_status(&1), &next(&1)
    case v do
      value_list when is_list(value_list) ->
        { :ok, value_list }
      other ->
        other
    end
  end

  # result has the { status, result } form
  # runs fun(result) if status matches the parameter
  # otherwise returns result
  def if_status(status, result, fun) do
    case result do
      { ^status, value } ->
        fun.(value)
      other ->
        other
    end
  end

  def is_status_ok?({ status, _ }), do: status == :ok
  def is_status_ok?({ status, _, _ }), do: status == :ok
  def is_status_ok?({ status, _, _, _ }), do: status == :ok


  # removes status from a Runner return value
  def strip_status({ _, a }), do: a
  def strip_status({ _, a, b }), do: { a, b }

  ## next/1
  # makes nested next calls until leafs are reached.
  # keeps the current scope and binding
  # returns { :ok, value } or { :exception, kind, reason, stacktrace }

  # PID and function variables sholdn't be next'd
  # TODO: functions with one argument would fall here too
  def next({ var, meta, mod }) when is_atom(var) and is_atom(mod) do
    expr = { var, meta, mod }
    case is_escaped?(atom_to_binary(var)) do
      true ->
        { :ok, expr }
      _ ->
        eval_change_state(expr)
    end
  end

  # anonymous functions
  # TODO: manage context changing 
  def next { :fn, meta, [[do: body]] } do
    next_body = wrap_next_call(body)
    { :ok, { :fn, meta, [[do: next_body]] }}
  end

  # case
  def next({ :case, _, [condition | [[do: clauses]]] }) do
    { :ok, condition_value } = next(condition)
    match_next(condition_value, clauses) # is there more than do?
  end

  # receive
  def next({ :receive, _, [[do: clauses]] }) do
    { :receive, received_value } = with_state &Evaluator.do_receive(&1)
    match_next(received_value, clauses) 
  end

  # receive-after
  def next({ :receive, _, [[do: do_clauses, after: after_clause]] }) do
    {:->, _, [{ [after_time], _, after_expr }]} = after_clause

    case with_state &Evaluator.do_receive(&1, after_time) do
      { :receive, received_value } ->
        match_next(received_value, do_clauses) 
      { :after, _ } ->
        next(after_expr) 
    end
  end

  # try
  def next({ :try, _, [clauses] }) do
    do_clause = clauses[:do]

    # variables defined on try block aren't accessible outside it
    # TODO: this is wrong, as only bindings from try aren't kept
    do_result = do_and_discard_state fn ->
      next(do_clause)
    end

    case do_result do
      { :exception, kind, reason, stacktrace } ->
        exception = { :exception, kind, reason, stacktrace }
        exception_next(exception, clauses[:rescue], clauses[:catch])
      { :ok, value } ->
        if clauses[:else] do
          match_next(value, clauses[:else])
        else
          do_result
        end
    end
  end

  # assignments
  def next({ :=, meta, [left | [right]] }) do
    if_status :ok, next(right), fn(right_value) ->
      eval_change_state({ :=, meta, [left | [right_value]] })
    end
  end

  # list of expressions
  def next({ type, meta, expr_list }) when is_list(expr_list) do
    expr = { type, meta, expr_list }

    do_or_expand expr, fn ->
      if_status :ok, map_next_while_ok(expr_list), fn(value_list) ->
        eval_change_state({ type, meta, value_list })
      end
    end
  end

  # other expressions are evaluated directly
  def next({ left, meta, right }) do
    expr = { left , meta, right }
    do_or_expand expr, fn ->
      eval_change_state(expr)
    end
  end

  # lists aren't escaped like tuples
  def next(expr_list) when is_list(expr_list) do
    map_next_while_ok expr_list
  end

  # ignore everything else (atoms, binaries, numbers, unescaped tuples, etc.)
  def next(other), do: { :ok, other }

  # pattern matching operator should evaluate clauses until
  # the first clause matching the condition is found
  def match_next(value, clauses) do
    matching_clause = change_state fn(state) ->
      Evaluator.find_match_clause(value, clauses, state)
    end
    
    if_status :ok, matching_clause, fn({ _, _, right }) ->
      result = next(right)

      change_state fn(state) ->
        Evaluator.initialize_clause_vars(clauses, state)
      end

      result
    end
  end

  def exception_next(exception, rescue_block, catch_block) do
    { :exception, kind, reason, stacktrace } = exception
    esc_stacktrace = Macro.escape stacktrace
    esc_reason = Macro.escape reason

    clauses = [do: quote do
      :erlang.raise(unquote(kind), unquote(esc_reason), unquote(esc_stacktrace))
    end]

    if rescue_block, do: clauses = 
      Keyword.put clauses, :rescue, wrap_next_call(rescue_block)
    if catch_block, do: clauses = 
      Keyword.put clauses, :catch, wrap_next_call(catch_block)

    try_expr = { :try, [context: Debugger.Evaluator, import: Kernel], [clauses] }

    if try_expr do
      do_and_discard_state fn ->
        with_state fn(state) ->
          result = Evaluator.eval_quoted(try_expr, state)
          { :ok, value, _ } = result
          value
        end
      end
    else
      exception
    end
  end

  def wrap_next_call({ :->, meta, clauses }) do
    wrap_clauses = Enum.map clauses, fn({ left, clause_meta, right }) ->
      { left, clause_meta, wrap_next_call(right) }
    end
    { :->, meta, wrap_clauses }
  end
  def wrap_next_call(expr) do
    esc_expr = Macro.escape expr

    quote do
      # TODO: are we using the proper scope?
      # put the current binding there
      PIDTable.start_link

      case PIDTable.get(self) do
        nil ->
          binding = Kernel.binding
          scope = __ENV__
        coord ->
          state = Coordinator.get_state(coord)
          binding = Keyword.merge(state.binding, Kernel.binding)
          scope = :elixir_scope.vars_from_binding(state.scope, binding)
      end
      
      PIDTable.start(self, binding, scope)
      return = Runner.next(unquote(esc_expr))
      PIDTable.finish(self)

      return
    end
  end
end
