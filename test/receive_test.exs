Code.require_file "test_helper.exs", __DIR__

defmodule ReceiveTest do
  use ExUnit.Case
  import Debugger

  ## receive
  defdebug receive_f1 do
    self <- :foo
    receive do
      :foo -> 10
    end
  end
  defdebug receive_f2 do
    self <- :bar
    receive do
      :foo -> 10
      _ -> 20
    end
  end
  defdebug receive_f3 do
    receive do
      after 1 -> 30
    end                        
  end
  test "receive" do
    assert 10 == receive_f1 
    assert 20 == receive_f2
    assert 30 == receive_f3 
  end

  defdebug vars_receive_f1 do
    self <- :foo
    receive do
      :foo ->
        a = 10
      :bar -> nil
    end
    a
  end
  defdebug vars_receive_f2 do
    self <- :bar
    receive do
      :foo ->
        b = 10
      _ -> 20
    end
    b
  end
  defdebug vars_receive_f3 do
    receive do
      :foo -> nil
    after
      1 -> c = 30
    end
    c
  end
  defdebug vars_receive_f4 do
    x = 1
    receive do
      :foo -> nil
    after
      x -> c = 30
    end
    c
  end

  test "binding variables on receive" do
    assert 10 == vars_receive_f1
    assert nil == vars_receive_f2
    assert 30 == vars_receive_f3
    assert 30 == vars_receive_f4
  end
end
