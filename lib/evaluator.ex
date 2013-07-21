defmodule Evaluator do

  defmacro defdebug(header, do: body) do
    { _, _, quoted_params } = header
    vars = Enum.map quoted_params, fn({ var, meta, module }) ->
      { var, ({ var, meta, module }) }
    end

    quote do
      def unquote(header) do
        evaluator_pid = spawn_link fn ->
          Evaluator.start(self, unquote(vars), __ENV__)
        end
 
        return_value = Evaluator.next(evaluator_pid, unquote(Macro.escape(body)))

        evaluator_pid <- { :done, return_value }
        return_value
      end
    end
  end
 
  def start(pid, binding, scope) do
    # TODO: evaluator_pid can get collide with other names
    scope = :elixir_scope.vars_from_binding(:elixir_scope.to_erl_env(scope), binding)
    pid_binding = Keyword.put_new(binding, :evaluator_pid, pid)
    loop(pid_binding, scope)
  end
  
  # sons need to be evaluated before parents
  def next(pid, expr) do
    pid <- { :next, expr, self }
    receive do
      { :ok, value } ->
        IO.inspect value
        value
    end
  end
 
  def emit_evaluator_pid do
    {:evaluator_pid, [], nil}
  end

  defp loop(binding, scope) do
    receive do
      { :next, expr, sender } ->
        { value, new_binding, new_scope } = Evaluator.eval(expr, binding, scope)

        IO.inspect value

        sender <- { :ok, value }
        loop(new_binding, new_scope)
 
      { :done, value } ->
        IO.inspect { :done, value }
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
 
    ev_pid = emit_evaluator_pid
    esc_condition = expand_expr(condition, scope)
    exp_condition = quote do: Evaluator.next(unquote(ev_pid), unquote esc_condition)

    exp_body = expand_expr(body, scope)
 
    { :case, meta, [exp_condition, [exp_body]] }
  end
  
  # pattern matching operator
  defp expand_expr({ :-> , meta, clauses }, scope) do
    ev_pid = emit_evaluator_pid
    exp_clauses = Enum.map(clauses, fn ({ left, clause_meta, right }) ->
      esc_right = expand_expr(right, scope)
      exp_right = quote do: Evaluator.next(unquote(ev_pid), unquote esc_right)
      { left, clause_meta, exp_right }
    end)
 
    { :->, meta, exp_clauses }
  end
 
  # expression list
  defp expand_expr({ type, meta, expr_list }, scope) when is_list(expr_list) do
    exp_expr_list = Enum.map expr_list, expand_expr(&1, scope)
 
    ev_pid = emit_evaluator_pid
    esc = Macro.escape { type, meta, exp_expr_list }
    quote do: Evaluator.next(unquote(ev_pid), unquote esc)
  end

  # leaf expressions
  defp expand_expr(expr, scope) do
    expr
  end
end

defmodule Foo do
  import Evaluator 

  Evaluator.defdebug bar(value) do
    value + 1
    #other = value + 1
    #if value do
    #  other
    #else
    #  :none
    #end
  end

  def baz(value) do 
    evaluator_pid = spawn_link fn ->
      Evaluator.start(self, [value: value], __ENV__)
    end
 
    return_value = Evaluator.next(evaluator_pid, quote do: value)

    evaluator_pid <- :done
    return_value
  end
end

Foo.bar 42
