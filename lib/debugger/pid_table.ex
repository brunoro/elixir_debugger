defmodule Debugger.PIDTable do
  use GenServer.Behaviour
  alias Debugger.Coordinator

  @server_name { :global, :pid_table }

  def start_link do
    :gen_server.start_link(@server_name, __MODULE__, [], [])
  end

  def init do
    { :ok, HashDict.new }
  end

  def handle_call({ :get, pid }, _sender, dict) do
    case dict[pid] do
      { coord, _count } ->
        { :reply, coord, dict }
      nil ->
        { :reply, nil, dict }
    end
  end

  # before function calls
  def handle_call({ :start, pid, binding, scope }, _sender, dict) do
    entry = case dict[pid] do
      { coord, count } ->
        # create new context
        Coordinator.push_stack(coord)
        state = Coordinator.get_state(coord)
        Coordinator.put_state(coord, state.binding(binding).scope(scope))

        { coord, count + 1 }
      nil ->
        { :ok, coord } = Coordinator.start_link(binding, scope)
        { coord, 0 }
    end

    { :reply, coord, Dict.put(dict, pid, entry) }
  end
 
  # after function calls
  def handle_cast({ :finish, pid }, dict) do
    new_dict = case dict[pid] do
      { coord, 0 } -> 
        Coordinator.done(coord)
        Dict.delete(dict, pid)
      { coord, count } ->
        # exit context
        Coordinator.pop_stack(coord)
        Dict.put(dict, pid, { coord, count - 1 })
    end

    { :noreply, new_dict }
  end

  def get(pid),                   do: :gen_server.call(@server_name, { :get, pid })
  def start(pid, binding, scope), do: :gen_server.call(@server_name, { :start, pid, binding, scope })
  def finish(pid),                do: :gen_server.cast(@server_name, { :finish, pid })
end