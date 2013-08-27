defmodule Debugger.UI do
  alias Debugger.Controller

  def start_link do
    self_pid = self
    pid = spawn_link(fn -> input_loop(self_pid) end)

    do_loop(pid)
  end

  def do_loop(input_pid) do
    input_pid <- :do_input
    wait_input(input_pid)
  end

  def wait_input(input_pid) do
    receive do
      { :input, line } ->
        proc_list = Controller.list
        tokens = line |> String.strip |> String.split
        command(tokens, proc_list)
    end

    do_loop(input_pid)
  end

  defp input_loop(ui_pid) do
    receive do
      :do_input ->
        ui_pid <- { :input, prompt }
    end
    input_loop(ui_pid)
  end

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


  def prompt do
    IO.gets("debugger> ") 
  end
end
