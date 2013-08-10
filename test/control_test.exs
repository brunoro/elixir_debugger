Code.require_file "test_helper.exs", __DIR__

defmodule ControlTest do
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

  ## case
  defdebug case_f1 do
    case 1 do
      2 -> false
      1 -> true
    end
  end
  defdebug case_f2 do
    case 1 do
      {x,y} -> false
      x -> true
    end
  end
  defdebug case_f3 do
    case {1,2} do; {3,4} -> false
      _ -> true
    end
  end

  test "case" do
    assert true == case_f1
    assert true == case_f2
    assert true == case_f3 
  end

  defdebug case_with_do_ambiguity_f1 do
    case atom_to_list(true) do
      _ -> true
    end
  end

  test "case with ambiguity" do
    assert true == case_with_do_ambiguity_f1
  end

  defdebug case_with_match_do_ambiguity_f1 do
    case x = atom_to_list(true) do
      _ -> true
    end
  end

  test "case with match and ambiguity" do
    assert true  == case_with_match_do_ambiguity_f1
  end

  defdebug case_with_unary_do_ambiguity_f1 do
    ! case atom_to_list(true) do
      _ -> true
    end
  end

  test "case with unary and ambiguity" do
    assert false == case_with_unary_do_ambiguity_f1 
  end

  defdebug multi_assigned_case_f1 do
    x = 1
    case true do
       true ->
          x = 2
          x = 3
      _ -> true
    end
    x
  end
  defdebug multi_assigned_case_f2 do
    x = 1
    case 1 do
      ^x -> 
        x = 2
        x = 3
      _ -> true
    end
    x
  end 
  defdebug multi_assigned_case_f3 do
    case true do
      true -> x = 1
      _ -> true
    end
    x
  end
  defdebug multi_assigned_case_f4 do
    case true do
      false -> x = 1
      _ -> true
    end
    x
  end

  test "multi-assigned case" do
    assert 3 == multi_assigned_case_f1
    assert 3 == multi_assigned_case_f2
    assert 1 == multi_assigned_case_f3
    assert nil == multi_assigned_case_f4
  end

  defmodule VarsTestCase do
    defdebug foo, do: 1
    defdebug bar(x) do
      foo = 1
      case x do
        true -> foo = 2
        false -> foo = foo
      end
      foo
    end
  end

  test "binding variables on case" do
    assert 1 == VarsTestCase.bar(false)
    assert 2 == VarsTestCase.bar(true)
    #test_helper:run_and_remove(F, ['Elixir.Bar']).
  end
end
