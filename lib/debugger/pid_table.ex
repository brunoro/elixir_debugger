defmodule Debugger.PIDTable do
  use GenServer.Behaviour
  alias Debugger.StateServer

  @server_name { :global, :pid_table }

  def start_link do
    :gen_server.start_link(@server_name, __MODULE__, [], [])
  end

  def init do
    { :ok, HashDict.new }
  end

  def handle_call({ :get, pid }, _sender, dict) do
    case dict[pid] do
      { state_server, _count } ->
        { :reply, state_server, dict }
      nil ->
        { :reply, nil, dict }
    end
  end

  # before function calls
  def handle_call({ :start, pid, binding, scope }, _sender, dict) do
    entry = case dict[pid] do
      { state_server, count } ->
        # create new context
        StateServer.push_stack(state_server)
        state = StateServer.get(state_server)
        StateServer.put(state_server, state.binding(binding).scope(scope))

        { state_server, count + 1 }
      nil ->
        { :ok, state_server } = StateServer.start_link(binding, scope)
        { state_server, 0 }
    end

    { :reply, state_server, Dict.put(dict, pid, entry) }
  end
 
  # after function calls
  def handle_cast({ :finish, pid }, dict) do
    new_dict = case dict[pid] do
      { state_server, 0 } -> 
        StateServer.done(state_server)
        Dict.delete(dict, pid)
      { state_server, count } ->
        # exit context
        StateServer.pop_stack(state_server)
        Dict.put(dict, pid, { state_server, count - 1 })
    end

    { :noreply, new_dict }
  end

  def get(pid),                   do: :gen_server.call(@server_name, { :get, pid })
  def start(pid, binding, scope), do: :gen_server.call(@server_name, { :start, pid, binding, scope })
  def finish(pid),                do: :gen_server.cast(@server_name, { :finish, pid })

end
