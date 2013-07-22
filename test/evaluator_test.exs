Code.require_file "test_helper.exs", __DIR__

defmodule EvaluatorTest do
  use ExUnit.Case
  import Evaluator
  
  defdebug if_case_fun(value) do
    other = value + 1
    if value do
      other
    else
      100 + other
    end
  end

  test "expand if into case expression" do
    assert if_case_fun(41) == 42
  end

  defdebug case_binding_fun(value) do
    case value do
      { :ok, other } ->
        other
      other ->
        other
    end
  end

  defdebug case_binding_scope_fun(value) do
    # var = { :ok, :foo }
    var = value
    case value do
      { :ok, var } ->
        var
      _ ->
        var 
    end
    var
  end

  test "match expressions can change binding" do
    assert case_binding_fun({ :ok, :bar }) == :bar
    assert case_binding_fun(:bar) == :bar
  end

  test "matching only affects current scope's binding" do
    assert case_binding_scope_fun({ :ok, :foo }) == { :ok, :foo }
    assert case_binding_scope_fun(:bar) == :bar
  end

 end
