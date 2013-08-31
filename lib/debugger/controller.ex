defmodule Debugger.Controller do
  use GenServer.Behaviour
  alias Debugger.Runner

  @server_name { :global, :controller }

  def start_link do
    :gen_server.start_link(@server_name, __MODULE__, [], [])
  end

  def init do
    { :ok, HashDict.new }
  end

  # The Controller keeps track of the expressions processes are
  # currently running, being notified through next.
  def handle_cast({ :next, pid, expr }, controller_state) do
    new_state = Dict.put(controller_state, pid, expr) 
    next_io_loop(pid, new_state)
    { :noreply, new_state }
  end
  
  def handle_call(:list, _sender, controller_state) do
    { :reply, controller_state, controller_state }
  end

  def list,            do: :gen_server.call(@server_name, :list)
  def next(pid, expr), do: :gen_server.cast(@server_name, { :next, pid, expr })

  # stuff pasted from the extinct ui.ex
  def command(["list"], controller_state) do
    Enum.each Enum.with_index(controller_state), fn({{ pid, expr }, index}) ->
      IO.puts "(#{index}) #{inspect pid}:\n\t#{Macro.to_string expr}"
    end
    :loop
  end
  def command(["step", index], controller_state) do
    case Enum.at(controller_state, String.to_integer index) do
      { :ok, { pid, _ }} -> 
        Runner.continue(pid)
      :error ->
        IO.puts "invalid index"
    end
    :exit
  end
  def command(_, _) do
    IO.puts "wat"
    :loop
  end

  def read_input do
    IO.gets("debugger> ") |> String.strip |> String.split
  end

  def next_io_loop(pid, controller_state) do
    cmd_return = read_input |> command(controller_state)
    case cmd_return do
      :loop ->
        next_io_loop(pid, controller_state)
      :exit ->
        :ok
    end
  end
end
