defmodule Evaluator do
  defmacro defdebug(header, do: body) do
    { _, _, quoted_params } = header
    vars = Enum.map quoted_params, fn({ var, meta, module }) ->
      { var, ({ var, meta, module }) }
    end

    quote do
      def unquote(header) do
        this = self

        pid = spawn_link fn ->
          Evaluator.start(this, unquote(vars), __ENV__)
        end
 
        return_value = Evaluator.next(pid, unquote(Macro.escape(body)))

        pid <- { :done, return_value }
        return_value
      end
    end
  end
 
  def start(pid, binding, scope) do
    scope = :elixir_scope.vars_from_binding(:elixir_scope.to_erl_env(scope), binding)
    Evaluator.loop(pid, binding, scope)
  end
  
  def loop(pid, binding, scope) do
    receive do
      { :eval, expr } ->
        #IO.inspect { :eval, expr }
        IO.puts "\n>> #{Macro.to_string expr}"
        { value, new_binding, new_scope } = Evaluator.eval(expr, binding, scope)

        pid <- { :ok, value }
        loop(pid, new_binding, new_scope)

      { :expand, expr } ->
        #IO.inspect { :expand, expr }
        IO.puts "\n>> #{Macro.to_string expr}"

        { _, meta, _ } = expr
        ex_scope = :elixir_scope.to_ex_env({ meta[:line], scope })
        expanded = Macro.expand_once(expr, ex_scope)

        pid <- { :ok, expanded }
        loop(pid, binding, scope)

      { :match, { value, clauses }} ->
        matching = Enum.find clauses, fn({ [left], meta, right }) ->
          exp_left = Evaluator.expand_var(left, {:_, [], Kernel}, value)
          { value, _, _ } = Evaluator.eval(exp_left, binding, scope)
          value
        end

        pid <- { :ok, matching }
        loop(pid, binding, scope)
 
      { :done, value } ->
        IO.inspect { :done, value }
    end
  end

  def apply_or_map(var, fun) do
    if is_list(var), do: Enum.map(var, fun), else: fun.(var)
  end

  def expand_var(var, var, value), do: value
  def expand_var({ left, meta, right }, var, value) do
    exp_left = apply_or_map(left, expand_var(&1, var, value))
    exp_right = apply_or_map(right, expand_var(&1, var, value))
    
    { exp_left, meta, exp_right }
  end
  def expand_var(expr, var, value), do: expr

  # TODO: use scope on eval?
  def eval(expr, binding, _) do
    :elixir.eval_quoted([expr], binding)
  end

  # Makes nested Evaluator.next calls until leafs are reached.
  # Evaluates leaf expressions by sending them to pid, which
  # keeps the current scope and binding

  # values shouldn't be evaluated
  def next(_, value) when is_number(value), do: value
  def next(_, value) when is_binary(value), do: value
  def next(_, value) when is_atom(value),   do: value

  # ifs (TODO: many others) should be macro expanded
  def next(pid, { :if, meta, expr }) do 
    expanded = Evaluator.request(pid, :expand, { :if, meta, expr })
    Evaluator.next(pid, expanded)
  end
  # TODO: same format for case, receive, try
  def next(pid, { :case, meta, expr }) do
    [condition | [clauses]] = expr 
 
    condition_value = Evaluator.next(pid, condition)
    IO.puts  Macro.to_string { :case, meta, expr }
    Evaluator.match_next(pid, condition_value, clauses[:do]) # is there more than do?
  end
  # On assignments only the left side is evaluated separately
  def next(pid, { :=, meta, [left | [right]] }) do
    right_value = Evaluator.request(pid, :eval, right)
    Evaluator.request(pid, :eval, { :=, meta, [left | [right_value]] })
  end
  def next(pid, { type, meta, expr_list }) when is_list(expr_list) do
    value_list = Enum.map(expr_list, Evaluator.next(pid, &1))
    Evaluator.request(pid, :eval, { type, meta, value_list })
  end
  def next(pid, expr) do
    Evaluator.request(pid, :eval, expr)
  end

  # pattern matching operator should evaluate clauses until
  # the first clause matching the condition is found
  def match_next(pid, value, { :-> , meta, clauses }) do
    matching_clause = 
      Evaluator.request(pid, :match, { value, clauses })
    { left, clause_meta, right } = matching_clause
    Evaluator.next(pid, right)
  end

  def request(pid, req, expr) do
    pid <- { req, expr }
    receive do
      { :ok, result } ->
        IO.inspect { :ok, result }
        result
    end
  end
end

defmodule Foo do
  import Evaluator 

  Evaluator.defdebug bar(value) do
    other = value + 1
    if value do
      other
    else
      100 + other
    end
  end
end

Foo.bar 42
