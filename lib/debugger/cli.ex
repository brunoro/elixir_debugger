defmodule Debugger.CLI do
  use GenServer.Behaviour

  @server_name { :global, :cli }

  def start_link do
    :gen_server.start_link(@server_name, __MODULE__, [], [])
  end

  def init do
    { :ok, nil }
  end

  def handle_cast({ :next, pid, expr }, state) do
    IO.puts "#{inspect pid}:\n#{Macro.to_string expr}"
    read_command(pid)

    { :noreply, state }
  end

  def prompt(pid) do
    IO.gets("debugger(#{inspect pid}) ") |> String.strip
  end

  def read_command(pid) do
    case prompt(pid) do
      "step" ->
        step(pid)
      other ->
        read_command(pid)
    end
  end

  def next(pid, expr), do: :gen_server.cast(@server_name, { :next, pid, expr })
  def step(pid), do: pid <- :go

end
