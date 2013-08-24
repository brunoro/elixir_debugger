defmodule Debugger.CLI do
  use GenServer.Behaviour

  @server_name { :global, :cli }

  def start_link do
    :gen_server.start_link(@server_name, __MODULE__, [], [])
  end

  def init do
    { :ok, nil }
  end

  def handle_cast({ :next, pid, expr }, nil) do
    IO.puts Macro.to_string expr
    read_command(pid)

    { :noreply, nil }
  end

  def read_command(pid) do
    case IO.gets "debug(#{inspect pid})>" do
      "step" ->
        step(pid)
      _other ->
        read_command(pid)
    end
  end

  def next(pid, expr), do: :gen_server.handle_cast(@server_name, { :next, pid, expr })
  def step(pid), do: pid <- :go

end
