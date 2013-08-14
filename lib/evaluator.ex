defmodule Debugger.Evaluator do
  alias Debugger.PIDName

  # how to evaluate expressions 
  # TODO: use scope on eval?
  def eval_quoted(expr, state) do
    try do
      { value, binding, scope } = :elixir.eval_quoted([expr], state.binding)

      # escape any pids
      { clean_value, new_state } = wrap_pid(value, state.binding(binding).scope(scope))
      { :ok, clean_value, new_state }
    catch
      kind, reason -> 
        { :exception, kind, reason, :erlang.get_stacktrace }
    end
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
    { :ok, Macro.expand_once(expr, ex_scope) }
  end

  def do_receive(state) do
    receive_code = quote do
      receive do
        value -> { :receive, value }
      end
    end
    
    { :ok, { status, value }, new_state } = eval_quoted(receive_code, state)
    { status, value, new_state }
  end
  def do_receive(state, after_time) do
    receive_code = quote do
      receive do
        value -> { :receive, value }
      after
        unquote(after_time) -> { :after, nil }
      end
    end
    
    { :ok, { status, value }, new_state } = eval_quoted(receive_code, state)
    { status, value, new_state }
  end

  # generates `unquote(lhs) -> unquote(Macro.escape clause)`
  def find_match_clause(value, clauses, state) do 
    clause_list = escape_clauses(clauses)

    match_clause_case = quote do
      case unquote(value) do
        unquote(clause_list)
      end
    end

    eval_quoted(match_clause_case, state)
  end

  def initialize_clause_vars({ :->, _meta, clauses }, state) do
    match_clause = { [:__initialize_clause_vars__], [], :ok }
    all_clauses = { :->, [], [match_clause | clauses] }

    match_clause_case = quote do
      case :__initialize_clause_vars__ do
        unquote(all_clauses)
      end
    end

    eval_quoted(match_clause_case, state)
  end

  def escape_clauses({ :->, meta, clauses }) do
   clause_list = Enum.map clauses, fn(clause) ->
      { left, _, _ } = clause
      esc_clause = Macro.escape clause

      { left, [], esc_clause }
    end

    { :->, meta, clause_list }
   end
end
