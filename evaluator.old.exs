defmodule Evaluator do
  # if should be expanded
  def exprdebug({ :if, meta, rest }) do 
    #exprdebug(Macro.expand({ :if, meta, rest }, __ENV__))
 
    [condition, clauses] = rest
 
    exp_condition = exprdebug(condition)
    exp_clauses = Enum.map(clauses, fn({k, v}) -> { k, exprdebug(v) } end)
 
    { :if, meta, [exp_condition, exp_clauses]}
  end
 
  # same format for case, receive, try
  def exprdebug({ :case, meta, expr }) do
    [condition | [dobody]] = expr # why [dobody] instead of dobody ?
    body = Keyword.get(dobody, :do)
 
    esc_condition = Macro.escape condition
    exp_condition = quote do: Evaluator.next(evaluator_pid, unquote esc_condition)
    exp_body = exprdebug(body)
 
    { :case, meta, [exp_condition, [exp_body]] }
  end
  
  # pattern matching operator
  def exprdebug({ :-> , meta, clauses }) do
    # what are x, y in { expr, x ,y } ?
    exp_clauses = Enum.map(clauses, fn ({ left, clause_meta, right }) ->
      esc_right = Macro.escape right
      exp_right = quote do: Evaluator.next(evaluator_pid, unquote esc_right)
      { left, clause_meta, exp_right }
    end)
 
    { :->, meta, exp_clauses }
  end
 
  # blocks
  def exprdebug({ :__block__, meta, stmt_list }) do
    expr_stmt_list = Enum.map stmt_list, exprdebug(&1)
 
    { :__block__, meta, expr_stmt_list }
  end
  
  # simple expressions
  def exprdebug(expr) do
    esc_expr = Macro.escape expr
    quote do: Evaluator.next(evaluator_pid, unquote esc_expr)
  end
 
  # injects Evaluator.next and Evaluator.start calls to instrument a function
  defmacro defdebug(header, do: body) do
    exp_body = exprdebug(body)
 
    # the only bindings available are the parameters
    { _, _, quoted_params } = header
    params = Enum.map quoted_params, fn({ var, meta, module }) ->
      { var, ({ var, meta, module }) }
    end
 
    quote do
      def(unquote(header)) do
        this = self
        evaluator_pid = spawn fn ->
          Evaluator.start(unquote(params), __ENV__, this)
        end
 
        unquote(exp_body)
        evaluator_pid <- :done
      end
    end
  end
 
  # process that receives expressions and evaluates them
  def start(binding, opts, parent) do
    receive do
      { :next, expr } ->
        IO.inspect expr
        { value, new_binding } = Code.eval_quoted(expr, binding, opts)
        IO.inspect value 
        parent <- { :ok, value }
        Evaluator.start(new_binding, opts, parent)
      other ->
        other
    end
  end
 
  # sends an expression to be evaluated 
  def next(pid, expr) do
    pid <- { :next, expr }
    receive do
      { :ok, value } ->
        value
    end
  end
 
  # try debugging a function
  def foobar do
    fun = quote do
      defdebug bar(value) do
        other = value + 1
        if value do
          other
        else
          :none
        end
      end
    end
 
    exp = Macro.expand(fun, __ENV__)
    mod = quote do
      defmodule Foo do
        import Evaluator
 
        unquote exp
      end
    end
 
    IO.puts Macro.to_string(mod)
    #Code.eval_quoted(mod)
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
 
Foo.bar 12
