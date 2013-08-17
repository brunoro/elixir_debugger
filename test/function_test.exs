Code.require_file "test_helper.exs", __DIR__

defmodule FunctionTest do
  use ExUnit.Case
  import Debugger

  defdebug sum_list(x) do
    Enum.reduce x, 0, fn(a, b) -> a + b end
  end

  test "pass functions as parameters" do
    assert sum_list([1, 1, 1]) == 3
    assert sum_list([1, 2, 3]) == 6
  end
  
  defdebug capt_f1 do
    cap = 10
    fun = fn ->
      cap + 32
    end
    fun.()
  end
  defdebug capt_f2 do
    cap = 5
    fun = fn(cap) ->
      cap + 32
    end
    fun.(10)
  end

  test "anon functions capture variables" do
    assert capt_f1 == 42
    assert capt_f2 == 42
  end

  defdebug scope_f1 do
    a = 1
    fun = fn ->
      a = 2
    end
    fun.()
    a
  end

  test "change context on function calls" do
    assert scope_f1 == 1
  end
end
