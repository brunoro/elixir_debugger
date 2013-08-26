defmodule Debugger.StateServer do
  use GenServer.Behaviour

  defrecord State, [binding: nil, scope: nil, stack: []]

  # public interface
  def start_link(binding, scope) do
    :gen_server.start_link(__MODULE__, State[binding: binding, scope: scope], [])
  end

  # gen_server methods
  def init(state) do
    scope = :elixir_scope.vars_from_binding(state.scope, state.binding)
    { :ok, state.scope(scope) }
  end
    
  ## handle_call
  def handle_call(:get, _from, state) do
    { :reply, state, state }
  end

  ## handle_cast
  def handle_cast(:done, state) do
    { :stop, :normal, state }
  end
  
  def handle_cast(:pop_stack, state) do 
    case state.stack do
      [] ->
        { :noreply, state }
      [{ binding, scope } | rest] ->
        { :noreply, State[binding: binding, scope: scope, stack: rest] }
    end
  end

  def handle_cast(:push_stack, state) do
    { :noreply, state.stack([{ state.binding, state.scope } | state.stack]) }
  end

  def handle_cast({ :put, new_state }, _state) do
    { :noreply, new_state }
  end

  # client functions
  def done(pid),             do: :gen_server.cast(pid, :done)
  def get(pid),              do: :gen_server.call(pid, :get)
  def put(pid, state),       do: :gen_server.cast(pid, { :put, state })

  def pop_stack(pid),        do: :gen_server.cast(pid, :pop_stack)
  def push_stack(pid),       do: :gen_server.cast(pid, :push_stack)
end
