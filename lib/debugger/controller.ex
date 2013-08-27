defmodule Debugger.Controller do
  use GenServer.Behaviour

  @server_name { :global, :controller }

  def start_link do
    :gen_server.start_link(@server_name, __MODULE__, [], [])
  end

  def init do
    { :ok, HashDict.new }
  end

  # The Controller keeps track of the expressions processes are
  # currently running, being notified through next.
  def handle_cast({ :next, pid, expr }, expr_table) do
    step(pid)
    { :noreply, Dict.put(expr_table, pid, expr) }
  end
  
  def handle_call(:list, _sender, expr_table) do
    { :reply, expr_table, expr_table }
  end

  def list,            do: :gen_server.call(@server_name, :list)
  def step(pid),       do: pid <- :go
  def next(pid, expr), do: :gen_server.cast(@server_name, { :next, pid, expr })

  # stuff pasted from the extinct ui.ex
  def command(["list"], proc_list) do
    Enum.reduce proc_list, 0, fn({ pid, expr }, index) ->
      IO.puts "(#{index}) #{inspect pid}:\n\t#{Macro.to_string expr}"
      index + 1
    end
  end
  def command(["step", index], proc_list) do
    case Enum.at(proc_list, index) do
      { :ok, { pid, _ }} -> 
        Controller.step(pid)
      :error ->
        IO.puts "invalid index\n"
    end
  end
  def command(_, _), do: IO.puts "wat\n"


end
