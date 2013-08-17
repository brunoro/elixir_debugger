Code.require_file "test_helper.exs", __DIR__

defmodule IfTest do
  use ExUnit.Case
  import Debugger

  ## if-else
  defdebug if_else_kv_args_f1, do: if(true, do: 1)
  defdebug if_else_kv_args_f2, do: if(false, do: 1)
  defdebug if_else_kv_args_f3, do: if(false, do: 1, else: 2)

  test "if-else with keyword list arguments" do
    assert 1 == if_else_kv_args_f1
    assert nil == if_else_kv_args_f2
    assert 2 == if_else_kv_args_f3
  end

  defdebug if_else_kv_blocks_f1 do
    if(false) do
      1
    else
      2
    end 
  end
  defdebug if_else_kv_blocks_f2 do
    if(false) do
      1
      3
    else
      2
    end
  end
  defdebug if_else_kv_blocks_f3 do
    if(false) do 1 else 2 end
  end
  defdebug if_else_kv_blocks_f4 do
    if(false) do 1; else 2; end
  end
  defdebug if_else_kv_blocks_f5 do
    if(false) do 1; else 2; 3; end
  end

  test "if-else with blocks" do
    assert 2 == if_else_kv_blocks_f1
    assert 2 == if_else_kv_blocks_f2
    assert 2 == if_else_kv_blocks_f3
    assert 2 == if_else_kv_blocks_f4
    assert 3 == if_else_kv_blocks_f5
  end

  defdebug vars_if_f1 do
    if foo = 1 do; true; else false; end; foo
  end
  defmodule VarsIfTest do
    defdebug foo, do: 2
    defdebug bar(x) do if x do; foo = 3; else foo = foo; end; foo; end
  end

  test "binding variables on if-else" do
    assert 1 == vars_if_f1
    assert 2 == VarsIfTest.bar(false)
    assert 3 == VarsIfTest.bar(true)
  end
  # test_helper.run_and_remove(F, ['Elixir.Bar']).

  defdebug multi_assigned_if_f1 do
    x = 1
    if true do
      x = 2
      x = 3
    else true
    end
    x
  end 
  defdebug multi_assigned_if_f2 do
    x = 1
    if true do
      ^x = 1
      x = 2
      x = 3
    else true
    end
    x
  end
  defdebug multi_assigned_if_f3 do
    if true do
      x = 1
    else true
    end
    x
  end
  defdebug multi_assigned_if_f4 do
    if false do
      x = 1
    else true
    end
    x
  end

  test "multiple assignments on if-else" do
    assert 3 == multi_assigned_if_f1
    assert 3 == multi_assigned_if_f2
    assert 1 == multi_assigned_if_f3 
    assert nil == multi_assigned_if_f4 
  end
end
