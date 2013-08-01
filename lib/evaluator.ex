defmodule Debugger.Evaluator do
  alias Debugger.PIDName

  # how to evaluate expressions 
  # TODO: use scope on eval?
  def eval_quoted(expr, state) do
    { value, binding, scope } = :elixir.eval_quoted([expr], state.binding)
    wrap_pid(value, state.binding(binding).scope(scope))
  end

  # add pids to binding with some name mangling
  # NO PIDS SHALL PASS!!
  def wrap_pid(pid, state) when is_pid(pid) do 
    var = pid |> PIDName.to_pid_name |> binary_to_atom
    new_binding = Keyword.put(state.binding, var, pid)

    {{ var, [], nil }, state.binding(new_binding) }
  end
  def wrap_pid(value, state), do: { value, state }

  # interface functions
  def expand(expr, state) do
    { _, meta, _ } = expr
    ex_scope = :elixir_scope.to_ex_env({ meta[:line] || 0, state.scope })
    Macro.expand_once(expr, ex_scope)
  end

  def do_receive(state) do
    receive_code = quote do
      receive do
        value -> { :receive, value }
      end
    end
    
    { result, _ } = Evaluator.eval_quoted(receive_code, state)
    result 
  end
  def do_receive(state, after_time) do
    receive_code = quote do
      receive do
        value -> { :receive, value }
      after
        unquote(after_time) -> :after
      end
    end
    
    { result, _ } = Evaluator.eval_quoted(receive_code, state)
    result 
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

  def initialize_clause_vars(clauses, state) do
    defined_vars = Enum.reduce clauses, HashSet.new, fn(clause, vars) ->
      { _, _, right } = clause
      clause_vars = collect_vars(right, vars)
    end

    match_binding = Enum.map defined_vars, &({ &1, nil })
    prefer_non_nil = fn(_k, v1, v2) -> v1 || v2 end

    state.binding(Keyword.merge(state.binding, match_binding, prefer_non_nil))
  end

  def collect_vars({ :=, _, [left | [right]] }, vars) do
    { var, _, _ } = left
    collect_vars(right, Set.put(vars, var))
  end
  def collect_vars({ _, _, expr_list }, vars) when is_list(expr_list) do
    Enum.reduce(expr_list, vars, &collect_vars(&1, &2))
  end
  # other expressions are evaluated directly
  def collect_vars(_, vars), do: vars
end
