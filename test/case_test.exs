Code.require_file "test_helper.exs", __DIR__

defmodule CaseTest do
  use ExUnit.Case
  import Debugger

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
