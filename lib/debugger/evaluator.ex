defmodule Debugger.Evaluator do
  import Debugger.Escape

  # how to evaluate expressions 
  # TODO: use scope on eval?
  def eval_quoted(expr, state) do
    try do
      { value, binding, scope } = :elixir.eval_quoted([expr], state.binding)
      new_scope = :elixir_scope.vars_from_binding(scope, binding)

      { :ok, value, state.binding(binding).scope(new_scope) }
    catch
      kind, reason -> 
        { :exception, kind, reason, :erlang.get_stacktrace }
    end
  end

  # add functions and pids to binding with some name mangling
  def escape_and_bind(thing, state) when is_pid(thing) or is_function(thing) do 
    var = thing |> escape |> binary_to_atom
    new_binding = Keyword.put(state.binding, var, thing)
    new_scope = :elixir_scope.vars_from_binding(state.scope, new_binding)

    {{ var, [], nil }, state.binding(new_binding).scope(new_scope) }
  end
  # star trek: deep escape 9
  def escape_and_bind(list, state) when is_list(list) do
    { l, s } = Enum.reduce list, { [], state }, fn(expr, { acc, old_state }) ->
      { esc, new_state } = escape_and_bind(expr, old_state)
      { [esc | acc], new_state }
    end
    { Enum.reverse(l), s }
  end
  def escape_and_bind(tuple, state) when is_tuple(tuple) do
    list = tuple_to_list(tuple)
    { esc_list, new_state } = escape_and_bind(list, state)
    esc_tuple = list_to_tuple(esc_list)
    { esc_tuple, new_state }
  end
  def escape_and_bind(value, state), do: { value, state }

  def escape_and_eval(expr, state) do
    { esc_value, esc_state } = escape_and_bind(expr, state)
    eval_quoted(esc_value, esc_state)
  end

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
    
    { :ok, { status, value }, new_state } = escape_and_eval(receive_code, state)
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
    
    { :ok, { status, value }, new_state } = escape_and_eval(receive_code, state)
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

    escape_and_eval(match_clause_case, state)
  end

  def initialize_clause_vars({ :->, _meta, clauses }, state) do
    match_clause = { [:__initialize_clause_vars__], [], :ok }
    all_clauses = { :->, [], [match_clause | clauses] }

    match_clause_case = quote do
      case :__initialize_clause_vars__ do
        unquote(all_clauses)
      end
    end

    escape_and_eval(match_clause_case, state)
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
