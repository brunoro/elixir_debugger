defmodule Debugger.Runner do
  alias Debugger.Coordinator
  alias Debugger.Evaluator
  alias Debugger.PIDName
  alias Debugger.PIDTable

  # functions manipulating state coming from Coordinator
  def change_state(fun) do
    coord = PIDTable.get(self)
    state = Coordinator.get_state(coord)
    { result, new_state } = fun.(state)

    Coordinator.put_state(coord, new_state)
    result
  end

  def eval_change_state(expr) do
    change_state &Evaluator.eval_quoted(expr, &1)
  end

  def with_state(fun) do
    coord = PIDTable.get(self)
    state = Coordinator.get_state(coord)
    fun.(state)
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

  # Makes nested next calls until leafs are reached.
  # keeps the current scope and binding
  # values shouldn't be evaluated
  def next(value) when is_number(value), do: value
  def next(value) when is_binary(value), do: value
  def next(value) when is_atom(value),   do: value
  
  # PID variables sholdn't be next'd
  # TODO: functions with one argument would fall here too
  def next({ var, meta, mod }) when is_atom(var) and is_atom(mod) do
    expr = { var, meta, mod }
    case PIDName.is_pid_name?(atom_to_binary(var)) do
      true ->
        expr
      _ ->
        eval_change_state(expr)
    end
  end

  # case
  def next({ :case, _, [condition | [[do: clauses]]] }) do
    condition_value = next(condition)
    match_next(condition_value, clauses) # is there more than do?
  end

  # receive
  def next({ :receive, _, [[do: clauses]] }) do
    received_value = with_state &Evaluator.do_receive(&1)
    match_next(received_value, clauses) 
  end

  # assignments
  def next({ :=, meta, [left | [right]] }) do
    right_value = next(right)

    eval_change_state({ :=, meta, [left | [right_value]] })
  end

  # list of expressions
  def next({ type, meta, expr_list }) when is_list(expr_list) do
    expr = { type, meta, expr_list }

    do_or_expand expr, fn ->
      value_list = Enum.map(expr_list, &next(&1))
      eval_change_state({ type, meta, value_list })
    end
  end

  # other expressions are evaluated directly
  def next(expr) do
    do_or_expand expr, fn ->
      eval_change_state(expr)
    end
  end

  # pattern matching operator should evaluate clauses until
  # the first clause matching the condition is found
  def match_next(value, { :-> , _, clauses }) do
    { _, _, right } = change_state fn(state) ->
      Evaluator.find_matching_clause(value, clauses, state)
    end
    next(right)
  end
end