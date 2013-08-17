Code.require_file "test_helper.exs", __DIR__

defmodule ProcessTest do
  use ExUnit.Case
  import Debugger

  defdebug spawn_f1 do
    pid = spawn fn -> self end
    pid != self
  end

  defdebug spawn_f2 do
    pid = spawn_link fn -> self end
    pid != self
  end

  test "spawning processes" do
    assert spawn_f1 == true
    assert spawn_f2 == true
  end

  defdebug msg_f1 do
    this = self
    pid = spawn fn ->
      this <- :msg
    end
    receive do
      :msg -> :ok
    after
      10 -> :fail
    end
  end

  test "message passing" do
    assert msg_f1 == :ok
  end
end
