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
end
