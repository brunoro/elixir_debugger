Code.require_file "test_helper.exs", __DIR__

defmodule StringTest do
  use ExUnit.Case
  import Debugger

  defdebug interpolate_pid do
    { self, inspect(self) }
  end

  test "string interpolation" do 
    { pid, bin } = interpolate_pid
    assert inspect(pid) == bin
  end
end
