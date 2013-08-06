Code.require_file "test_helper.exs", __DIR__

defmodule CoordinatorTest do
  use ExUnit.Case
  alias Debugger.Coordinator

  test "put/get state" do
    binding = [a: 1]
    scope = :elixir_scope.to_erl_env(__ENV__)
    { :ok, coord } = Coordinator.start_link(binding, scope)

    state = Coordinator.get_state(coord)
    same_scope = :elixir_scope.vars_from_binding(scope, binding)
    same_state = Coordinator.State[binding: binding, scope: same_scope]
    assert state == same_state

    new_binding = [a: 1, b: 2]
    new_scope = :elixir_scope.vars_from_binding(scope, new_binding)
    new_state = state.binding(new_binding).scope(new_scope)
    Coordinator.put_state(coord, new_state)

    assert new_state == Coordinator.get_state(coord)
  end

  test "push/pop state stack" do
    binding = [a: 1]
    scope = :elixir_scope.to_erl_env(__ENV__)
    { :ok, coord } = Coordinator.start_link(binding, scope)

    state = Coordinator.get_state(coord)
    Coordinator.push_stack(coord)
    push_state = Coordinator.get_state(coord)

    assert state.binding == push_state.binding
    assert state.scope == push_state.scope
    assert state.stack != push_state.stack

    Coordinator.put_state(coord, push_state.binding([a: 1, b: 2]).scope(scope))
    new_state = Coordinator.get_state(coord)
    assert new_state.binding[:b] == 2

    Coordinator.pop_stack(coord)
    old_state = Coordinator.get_state(coord)

    assert state == old_state
    assert old_state.binding[:b] == nil
    
    # can't pop more the stack
    Coordinator.pop_stack(coord)
    assert old_state == Coordinator.get_state(coord)
  end
end