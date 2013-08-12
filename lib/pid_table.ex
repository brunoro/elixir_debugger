defmodule Debugger.PIDTable do
  use GenServer.Behaviour
  alias Debugger.Coordinator

  def start_link do
    :gen_server.start_link({ :local, :pid_table }, __MODULE__, [], [])
  end

  def init do
    { :ok, HashDict.new }
  end
 
  def handle_call({ :get, pid }, _sender, dict) do
    { coord, _count } = dict[pid]
    { :reply, coord, dict }
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
      { coord, 1 } -> 
        Coordinator.done(coord)
        Dict.delete(dict, pid)
      { coord, count } ->
        # exit context
        Coordinator.pop_stack(coord)
        Dict.put(dict, pid, { coord, count - 1 })
    end

    { :noreply, new_dict }
  end

  def get(pid),                   do: :gen_server.call(:pid_table, { :get, pid })
  def start(pid, binding, scope), do: :gen_server.call(:pid_table, { :start, pid, binding, scope })
  def finish(pid),                do: :gen_server.cast(:pid_table, { :finish, pid })
end
