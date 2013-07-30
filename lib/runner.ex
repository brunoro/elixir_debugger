defmodule Debugger.Runner do
  alias Debugger.Coordinator, as: Coordinator

  # how to evaluate expressions 
  # TODO: use scope on eval?
  def eval_quoted(expr, state) do
    { value, binding, scope } = :elixir.eval_quoted([expr], state.binding)
    wrap_pid(value, state.binding(binding).scope(scope))
  end

  # add pids to binding with some name mangling
  # NO PIDS SHALL PASS!!
  def wrap_pid(pid, state) when is_pid(pid) do 
    var = binary_to_atom(pid_name(pid))
    new_binding = Keyword.put(state.binding, var, pid)

    {{ var, [], Elixir }, state.binding(new_binding) }
  end
  def wrap_pid(value, state), do: { value, state }

  defp pid_name_prefix, do: "__PID_"

  # #PID<0.49.0> -> :"__PID_0_49_0__"
  def pid_name(pid) do
    esc_pid = pid |> pid_to_list 
                  |> to_binary 
                  |> String.lstrip("<")
                  |> String.rstrip(">")
                  |> String.replace(".", "_") 

    "#{pid_name_prefix}#{esc_pid}__"
  end
  def is_pid_name?(bin), do: String.starts_with?(bin, pid_name_prefix)

  # functions manipulating state
  def change_state(pid, fun) do
    state = Coordinator.get_state(pid)
    { result, new_state } = fun.(state)

    Coordinator.put_state(pid, new_state)
    result
  end

  def eval_change_state(pid, expr) do
    change_state pid, fn(state) ->
      eval_quoted(expr, state)
    end
  end

  def find_matching_clause(value, clauses, state) do 
    # generates `unquote(lhs) -> unquote(Macro.escape clause)`
    clause_list = Enum.map clauses, fn(clause) ->
      { left, _, _ } = clause
      esc_clause = Macro.escape clause

      { left, [], esc_clause }
    end

    # if no clause is matched return :nomatch
    nil_clause = {[{:_, [], Elixir}], [], :nomatch}
    all_clauses = { :->, [], List.concat(clause_list, [nil_clause]) }

    match_clause_case = quote do
      case unquote(value) do
        unquote(all_clauses)
      end
    end
    
    eval_quoted(match_clause_case, state)
  end

  def expand(pid, expr) do
    state = Coordinator.get_state(pid)
    { _, meta, _ } = expr
    ex_scope = :elixir_scope.to_ex_env({ meta[:line] || 0, state.scope })
    Macro.expand_once(expr, ex_scope)
  end

  def do_receive(pid) do
    state = Coordinator.get_state(pid)
    receive_code = quote do
      receive do
        value -> value
      end
    end
    eval_quoted(receive_code, state)
  end

  # expansions that lead to case-like expressions should be kept
  defp do_or_expand(pid, expr, fun) do 
    expanded = expand(pid, expr)
    case expanded do
      { :case, _, _ } ->
        next(pid, expanded)
      _ ->
        fun.()
    end
  end

  # Makes nested next calls until leafs are reached.
  # Evaluates leaf expressions by sending them to pid, which
  # keeps the current scope and binding

  # values shouldn't be evaluated
  def next(_, value) when is_number(value), do: value
  def next(_, value) when is_binary(value), do: value
  def next(_, value) when is_atom(value),   do: value
  
  # PID variables sholdn't be next'd
  # TODO: functions with one argument would fall here too
  def next(pid, { var, meta, mod }) when is_atom(var) and is_atom(mod) do
    expr = { var, meta, mod }
    case is_pid_name?(atom_to_binary(var)) do
      true ->
        expr
      _ ->
        eval_change_state(pid, expr)
    end
  end

  # case
  def next(pid, { :case, _, [condition | [[do: clauses]]] }) do
    condition_value = next(pid, condition)
    match_next(pid, condition_value, clauses) # is there more than do?
  end

  # receive
  def next(pid, { :receive, _, [[do: clauses]] }) do
    received_value = do_receive(pid)
    match_next(pid, received_value, clauses) 
  end

  # assignments
  def next(pid, { :=, meta, [left | [right]] }) do
    right_value = next(pid, right)

    eval_change_state(pid, { :=, meta, [left | [right_value]] })
  end

  # list of expressions
  def next(pid, { type, meta, expr_list }) when is_list(expr_list) do
    expr = { type, meta, expr_list }

    do_or_expand pid, expr, fn ->
      value_list = Enum.map(expr_list, next(pid, &1))
      eval_change_state(pid, { type, meta, value_list })
    end
  end

  # other expressions are evaluated directly
  def next(pid, expr) do
    do_or_expand pid, expr, fn ->
      eval_change_state(pid, expr)
    end
  end

  # pattern matching operator should evaluate clauses until
  # the first clause matching the condition is found
  def match_next(pid, value, { :-> , _, clauses }) do
    { _, _, right } = change_state pid, fn(state) ->
      find_matching_clause(value, clauses, state)
    end
    next(pid, right)
  end
end
