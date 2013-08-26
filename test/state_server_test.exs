Code.require_file "test_helper.exs", __DIR__

defmodule StateServerTest do
  use ExUnit.Case
  alias Debugger.StateServer

  test "put/get state" do
    binding = [a: 1]
    scope = :elixir_scope.to_erl_env(__ENV__)
    { :ok, state_server } = StateServer.start_link(binding, scope)

    state = StateServer.get(state_server)
    same_scope = :elixir_scope.vars_from_binding(scope, binding)
    same_state = StateServer.State[binding: binding, scope: same_scope]
    assert state == same_state

    new_binding = [a: 1, b: 2]
    new_scope = :elixir_scope.vars_from_binding(scope, new_binding)
    new_state = state.binding(new_binding).scope(new_scope)
    StateServer.put(state_server, new_state)

    assert new_state == StateServer.get(state_server)
  end

  test "push/pop state stack" do
    binding = [a: 1]
    scope = :elixir_scope.to_erl_env(__ENV__)
    { :ok, state_server } = StateServer.start_link(binding, scope)

    state = StateServer.get(state_server)
    StateServer.push_stack(state_server)
    push_state = StateServer.get(state_server)

    assert state.binding == push_state.binding
    assert state.scope == push_state.scope
    assert state.stack != push_state.stack

    StateServer.put(state_server, push_state.binding([a: 1, b: 2]).scope(scope))
    new_state = StateServer.get(state_server)
    assert new_state.binding[:b] == 2

    StateServer.pop_stack(state_server)
    old_state = StateServer.get(state_server)

    assert state == old_state
    assert old_state.binding[:b] == nil
    
    # can't pop more the stack
    StateServer.pop_stack(state_server)
    assert old_state == StateServer.get(state_server)
  end
end
