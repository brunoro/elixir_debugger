Code.require_file "test_helper.exs", __DIR__

defmodule EvaluatorTest do
  use ExUnit.Case

  test "the truth" do
    assert(true)
  end
  
  test "defdebug simple function" do
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
