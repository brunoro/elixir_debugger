defmodule Debugger.Runner do
  alias Debugger.Coordinator
  alias Debugger.Evaluator
  alias Debugger.PIDName
  alias Debugger.PIDTable

  # functions manipulating state coming from Coordinator
  def change_state(fun) do
    coord = PIDTable.get(self)
    state = Coordinator.get_state(coord)

    case fun.(state) do
      # ok, receive, after
      { status, result, new_state } ->
        Coordinator.put_state(coord, new_state)
        { status, result }
      # rescue, catch
      { status, result } ->
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
      { status, result, _state } ->
        { status, result }
      { status, result } ->
        { status, result }
    end
  end

  def eval_with_state(expr) do
    with_state &Evaluator.eval_quoted(expr, &1)
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
  def map_filter_while(col, condition, filter, fun) do 
    ret = do_map_filter_while(col, condition, filter, fun, [])
    case ret do
      list when is_list(list) ->
        Enum.reverse list
      other ->
        other
    end
  end

  defp do_map_filter_while([], _con, _fil, _fun, acc), do: acc
  defp do_map_filter_while([h | t], con, fil, fun, acc) do
    fh = fun.(h)
    if con.(fh) do
      do_map_filter_while(t, con, fil, fun, [fil.(fh) | acc])
    else
      fh
    end
  end

  # maps next/1 while status returned is :ok, otherwise returns the
  # failing element of the list with its status
  def map_next_while_ok(expr_list) do
    v = map_filter_while expr_list, &is_status_ok?(&1), &strip_status(&1), &next(&1)
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

  # Runner functions can return either 
  # { status, result } or { status, result, state }
  def is_status_ok?({ status, _ }), do: status == :ok
  def is_status_ok?({ status, _, _ }), do: status == :ok

  # removes status from a Runner return value
  def strip_status({ _, a }), do: a
  def strip_status({ _, a, b }), do: { a, b }

  # Makes nested next calls until leafs are reached.
  # keeps the current scope and binding

  # PID variables sholdn't be next'd
  # TODO: functions with one argument would fall here too
  def next({ var, meta, mod }) when is_atom(var) and is_atom(mod) do
    expr = { var, meta, mod }
    case PIDName.is_pid_name?(atom_to_binary(var)) do
      true ->
        { :ok, expr }
      _ ->
        eval_change_state(expr)
    end
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
  def next({ :try, _, [[do: do_clause, rescue: rescue_clauses]] }) do
    case next(do_clause) do
      { :rescue, exception } ->
        rescue_next(exception, rescue_clauses) 
      { :ok, value } ->
        value
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
  def match_next(value, { :-> , _, clauses }) do
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

  # pattern matching operator should evaluate clauses until
  # the first clause matching the condition is found
  def rescue_next(exception, { :-> , _, clauses }) do
    matching_clause = change_state fn(state) ->
      Evaluator.find_rescue_clause(exception, clauses, state)
    end
    
    if_status :ok, matching_clause, fn({ _, _, right }) ->
      next(right)
    end
  end
end
