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
        IO.inspect { :eval, expr }
        { value, new_binding, new_scope } = Evaluator.eval(expr, binding, scope)

        pid <- { :ok, value }
        loop(pid, new_binding, new_scope)
 
      { :done, value } ->
        IO.inspect { :done, value }
    end
  end

  # TODO: use scope on eval
  def eval(expr, binding, _) do
    :elixir.eval_quoted([expr], binding)
  end

  # Makes nested Evaluator.next calls until leafs are reached.
  # Evaluates leaf expressions by sending them to pid, which
  # keeps the current scope and binding
  # TODO: expand if and add case clauses

  # On assignments only the left side is evaluated separately
  def next(pid, { :=, meta, [left | [right]] }) do
    right_value = Evaluator.dispatch_eval(pid, right)
    Evaluator.dispatch_eval(pid, { :=, meta, [left | [right_value]] })
  end
  def next(pid, { type, meta, expr_list }) when is_list(expr_list) do
    value_list = Enum.map(expr_list, Evaluator.next(pid, &1))
    Evaluator.dispatch_eval(pid, { type, meta, value_list })
  end
  def next(pid, expr) do
    Evaluator.dispatch_eval(pid, expr)
  end

  def dispatch_eval(pid, expr) do
    pid <- { :eval, expr }
    receive do
      { :ok, value } ->
        IO.inspect { :ok, value }
        value
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
      other + 1
    end
  end
end

Foo.bar 42
