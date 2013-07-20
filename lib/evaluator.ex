defmodule Evaluator do

  defmacro binding do
    __CALLER__.vars
  end

  defmacro defdebug(header, do: body) do
    quote do
      def unquote(header) do
        pid = spawn_link fn ->
          Evaluator.start(self, Evaluator.binding, __ENV__)
        end
 
        return_value = Evaluator.next(pid, unquote(Macro.escape(body)))

        pid <- :done
        return_value
      end
    end
  end
 
  def start(pid, binding, scope) do
    #scope = :elixir_scope.vars_from_binding(:elixir_scope.to_erl_env(scope), binding)
    loop(pid, binding, scope)
  end
  
  def next(pid, expr) do
    pid <- { :next, expr }
    receive do
      { :ok, value } ->
        value
    end
  end
 
  defp loop(pid, binding, scope) do
    receive do
      { :next, expr } ->
        expanded = expand_expr(expr, scope)
        IO.puts Macro.to_string expanded
        { value, status, new_binding } = :elixir.eval_quoted(expanded, binding)

        IO.inspect Macro.to_string expr
        IO.inspect new_binding
        loop(pid, binding, scope)
 
      { :done, value } ->
        IO.inspect value
    end
  end

  # if (TODO: and other expressions) should be expanded
  defp expand_expr({ :if, meta, expr }, scope) do 
    """
    [condition, clauses] = rest
 
    exp_condition = expand_expr(condition, scope)
    exp_clauses = Enum.map clauses, fn({k, v}) -> 
      { k, expand_expr(v, scope) } 
    end
 
    { :if, meta, [exp_condition, exp_clauses]}
    """
    
    expanded = Macro.expand({ :if, meta, expr }, __ENV__)
    expand_expr(expanded, scope)
  end
 
  # same format for case, receive, try
  defp expand_expr({ :case, meta, expr }, scope) do
    [condition | [dobody]] = expr # why [dobody] instead of dobody ?
    body = Keyword.get(dobody, :do)
 
    esc_condition = Macro.escape condition
    exp_condition = quote do: Evaluator.next(evaluator_pid, unquote esc_condition)

    exp_body = expand_expr(body, scope)
 
    { :case, meta, [exp_condition, [exp_body]] }
  end
  
  # pattern matching operator
  defp expand_expr({ :-> , meta, clauses }, scope) do
    # what are x, y in { expr, x ,y } ?
    exp_clauses = Enum.map(clauses, fn ({ left, clause_meta, right }) ->
      esc_right = Macro.escape right
      exp_right = quote do: Evaluator.next(evaluator_pid, unquote esc_right)
      { left, clause_meta, exp_right }
    end)
 
    { :->, meta, exp_clauses }
  end
 
  # blocks
  defp expand_expr({ type, meta, expr_list }, scope) when is_list(expr_list) do
    exp_expr_list = Enum.map expr_list, expand_expr(&1, scope)
 
    esc = Macro.escape { type, meta, exp_expr_list }
    quote do: Evaluator.next(evaluator_pid, unquote esc)
  end
  
  # leaf expressions
  defp expand_expr(expr, scope) do
    esc_expr = Macro.escape expr
    quote do: Evaluator.next(evaluator_pid, unquote esc_expr)
  end
end

defmodule Foo do
  import Evaluator 

  Evaluator.defdebug bar(value) do
    other = value + 1
    if value do
      other
    else
      :none
    end
  end
end

Foo.bar 1
