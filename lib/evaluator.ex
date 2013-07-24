defmodule Evaluator do
  use GenServer.Behaviour

  defmacro defdebug(header, do: body) do
    # TODO: binding retrieved via __CALLER__ had all variables as nil
    { _, _, quoted_params } = header
    vars = Enum.map quoted_params, fn({ var, meta, module }) ->
      { var, ({ var, meta, module }) }
    end

    quote do
      def unquote(header) do
        { :ok, pid }
          = :gen_server.start_link(Evaluator, { unquote(vars), __ENV__ }, [])
        return_value = Evaluator.next(pid, unquote(Macro.escape(body)))

        Evaluator.done(pid)
        return_value
      end
    end
  end
 
  def init({ binding, scope }) do
    scope = :elixir_scope.vars_from_binding(:elixir_scope.to_erl_env(scope), binding)
    { :ok, { binding, scope }}
  end
  
  def handle_call({ :eval, expr }, _from, { binding, scope }) do
    { value, new_binding, new_scope } = Evaluator.eval_quoted(expr, binding, scope)
    { :reply, value, { new_binding, new_scope }}
  end
 
  def handle_call({ :expand, expr }, _from, { binding, scope }) do
    { _, meta, _ } = expr
    # TODO: WOT M8, meta[:line] is nil??
    ex_scope = :elixir_scope.to_ex_env({ meta[:line] || 0, scope })
    expanded = Macro.expand_once(expr, ex_scope)
 
    { :reply, expanded, { binding, scope }}
    end

  def handle_call({ :match, { value, clauses }}, _from, { binding, scope }) do
    { matching, new_binding, new_scope } = 
      Evaluator.find_matching_clause(value, clauses, binding, scope)

    # TODO: is this right? scope should be specific to match operation
    { :reply, matching, { new_binding, new_scope }}
  end
 
  def handle_cast(:done, state) do
    { :stop, :normal, state }
  end

  # TODO: should we raise an exception here?
  def find_matching_clause(_, [], binding, scope) do
    { nil, binding, scope }
  end
  def find_matching_clause(value, [clause | rest], binding, scope) do 
    # does it match?
    { [left], _, _ } = clause
    clause_test = quote do
      case unquote(value) do
        unquote(left) ->
          true
        _ ->
          false
      end
    end
    { bool, new_binding, new_scope } = Evaluator.eval_quoted(clause_test, binding, scope)

    # if it does we can send it back
    if bool do
      { clause, new_binding, new_scope }
    else
      Evaluator.find_matching_clause(value, rest, binding, scope)
    end
  end

  # TODO: use scope on eval?
  def eval_quoted(expr, binding, _) do
    :elixir.eval_quoted([expr], binding)
  end

  # client functions
  def done(pid), do: :gen_server.cast(pid, :done)
  def eval(pid, expr), do: :gen_server.call(pid, { :eval, expr })
  def expand(pid, expr), do: :gen_server.call(pid, { :expand, expr })
  def match(pid, value, clauses), do: :gen_server.call(pid, { :match, { value, clauses }})

  # Makes nested Evaluator.next calls until leafs are reached.
  # Evaluates leaf expressions by sending them to pid, which
  # keeps the current scope and binding

  # expansions that lead to case-like expressions should be kept
  defp do_or_expand(pid, expr, fun) do 
    expanded = Evaluator.expand(pid, expr)
    case expanded do
      { :case, _, _ } ->
        Evaluator.next(pid, expanded)
      _ ->
        fun.()
    end
  end

  # values shouldn't be evaluated
  def next(_, value) when is_number(value), do: value
  def next(_, value) when is_binary(value), do: value
  def next(_, value) when is_atom(value),   do: value

  # TODO: same format for case, receive, try
  #       can case be redefined?
  def next(pid, { :case, _, body }) do
    [condition | [clauses]] = body
 
    condition_value = Evaluator.next(pid, condition)
    Evaluator.match_next(pid, condition_value, clauses[:do]) # is there more than do?
  end

  # On assignments only the right side is evaluated separately
  def next(pid, { :=, meta, [left | [right]] }) do
    right_value = Evaluator.next(pid, right)
    Evaluator.eval(pid, { :=, meta, [left | [right_value]] })
  end

  def next(pid, { type, meta, expr_list }) when is_list(expr_list) do
    expr = { type, meta, expr_list }

    do_or_expand pid, expr, fn ->
      value_list = Enum.map(expr_list, Evaluator.next(pid, &1))
      Evaluator.eval(pid, { type, meta, value_list })
    end
  end

  def next(pid, expr) do
    do_or_expand pid, expr, fn ->
      Evaluator.eval(pid, expr)
    end
  end

  # pattern matching operator should evaluate clauses until
  # the first clause matching the condition is found
  def match_next(pid, value, { :-> , _, clauses }) do
    matching_clause = Evaluator.match(pid, value, clauses)
    { _, _, right } = matching_clause
    Evaluator.next(pid, right)
  end
end
