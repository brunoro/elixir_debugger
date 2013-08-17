Code.require_file "test_helper.exs", __DIR__

defmodule TryTest do
  use ExUnit.Case
  import Debugger

  ## try
  defdebug try_f1 do
    try do
      :foo.bar
    catch
      :error, :undef -> 2
    end
  end

  defdebug try_f2 do
    try do
      x = 1 + "a"
    rescue
      ArithmeticError -> 1
    end
  end

  test "try" do
    assert 2 == try_f1
    assert 1 == try_f2
  end

  # rescue only runtime errors
  defdebug try_rescue_f1 do
    try do
      raise "some error"
    rescue
      RuntimeError -> :rescue
    end
  end
  # rescue runtime and argument errors
  defdebug try_rescue_f2 do
    try do
      raise "some error"
    rescue
      [RuntimeError, ArgumentError] -> :rescue
    end
  end
  # rescue and assign to x
  defdebug try_rescue_f3 do
    try do
      raise "message"
    rescue
      x in [RuntimeError] ->
        # all exceptions have a message
        x.message
    end
  end

  test "try-rescue" do
    assert :rescue == try_rescue_f1
    assert :rescue == try_rescue_f2
    assert "message" == try_rescue_f3
  end

  defdebug try_else_f1 do
    try do
      1
    else 
      2 -> false
      1 -> true
    rescue
      ErlangError -> nil
    end
  end
  defdebug try_else_f2 do
    try do
      1
    else 
      {x,y} -> false
      x -> true
    rescue
      ErlangError -> nil
    end
  end
  defdebug try_else_f3 do
    try do
      {1,2}
    else 
      {3,4} -> false
      _ -> true
    rescue
      ErlangError -> nil
    end
  end
  
  test "try-else" do
    assert true == try_else_f1
    assert true == try_else_f2
    assert true == try_else_f3
  end
end
