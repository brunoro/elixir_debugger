defmodule Debugger.Runner do
  import Debugger.PidName

  alias Debugger.Coordinator
  alias Debugger.Evaluator

  # functions manipulating state coming from Coordinator
  def change_state(pid, fun) do
    state = Coordinator.get_state(pid)
    { result, new_state } = fun.(state)

    Coordinator.put_state(pid, new_state)
    result
  end

  def eval_change_state(pid, expr) do
    change_state pid, Evaluator.eval_quoted(expr, &1)
  end

  def with_state(pid, fun) do
    state = Coordinator.get_state(pid)
    fun.(state)
  end

  def eval_with_state(pid, expr) do
    with_state pid, Evaluator.eval_quoted(expr, &1)
  end

  # expansions that lead to case-like expressions should be kept
  defp do_or_expand(pid, expr, fun) do 
    expanded = with_state pid, Evaluator.expand(expr, &1)

    case expanded do
      { :case, _, _ } ->
        next(pid, expanded)
      _ ->
        fun.()
    end
  end

  # Makes nested next calls until leafs are reached.
  # keeps the current scope and binding
  # values shouldn't be evaluated
  def next(_, value) when is_number(value), do: value
  def next(_, value) when is_binary(value), do: value
  def next(_, value) when is_atom(value),   do: value
  
  # PID variables sholdn't be next'd
  # TODO: functions with one argument would fall here too
  def next(pid, { var, meta, mod }) when is_atom(var) and is_atom(mod) do
    expr = { var, meta, mod }
    case Pid.is_pid_name?(atom_to_binary(var)) do
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
    received_value = with_state pid, Evaluator.do_receive(&1)
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
      Evaluator.find_matching_clause(value, clauses, state)
    end
    next(pid, right)
  end
end
