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
    { :reply, dict[pid], dict }
  end

  def handle_call({ :put, pid, binding, scope }, _sender, dict) do
    { :ok, coordinator } = Coordinator.start_link(binding, scope)
    { :reply, coordinator, Dict.put(dict, pid, coordinator) }
  end
 
  def handle_cast({ :delete, pid }, dict) do
    coord = dict[pid]
    if coord, do: Coordinator.done(coord)
    { :noreply, Dict.delete(dict, pid) }
  end

  def get(pid),                 do: :gen_server.call(:pid_table, { :get, pid })
  def put(pid, binding, scope), do: :gen_server.call(:pid_table, { :put, pid, binding, scope })
  def delete(pid),              do: :gen_server.cast(:pid_table, { :delete, pid })
end
