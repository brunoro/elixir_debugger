defmodule Debugger.Coordinator do
  use GenServer.Behaviour

  defrecord State, [binding: nil, scope: nil, stack: []]

  def init(state) do
    scope = :elixir_scope.vars_from_binding(state.scope, state.binding)
    { :ok, state.scope(scope) }
  end
    
  # casts
  def handle_cast(:done, state) do
    { :stop, :normal, state }
  end
  
  def handle_cast(:pop_stack, State[stack: [state]]), do: { :noreply, state }
  def handle_cast(:pop_stack, State[stack: [old_state | _]]) do
    { :noreply, old_state }
  end

  def handle_cast(:push_stack, state) do
    { :noreply, state.stack([state | state.stack]) }
  end

  def handle_cast({ :put_state, new_state }, _state) do
    { :noreply, new_state }
  end

  # calls
  def handle_call(:get_state, _from, state) do
    { :reply, state, state }
  end

  # client functions
  def done(pid),             do: :gen_server.cast(pid, :done)
  def get_state(pid),        do: :gen_server.call(pid, :get_state)
  def put_state(pid, state), do: :gen_server.cast(pid, { :put_state, state })

  def pop_stack(pid),        do: :gen_server.cast(pid, :pop_stack)
  def push_stack(pid),       do: :gen_server.cast(pid, :push_stack)
end
