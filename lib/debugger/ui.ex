defmodule Debugger.UI do
  alias Debugger.Controller

  def start_link do
    loop
  end

  def loop do
    proc_list = Controller.list
    case prompt do
      ["list"] ->
        Enum.reduce proc_list, 0, fn({ pid, expr }, index) ->
          IO.puts "(#{index}) #{inspect pid}:\n\t#{Macro.to_string expr}"
          index + 1
        end

      ["step", index] ->
        case Enum.at(proc_list, index) do
          { :ok, { pid, _ }} -> 
            Controller.step(pid)
          :error ->
            IO.puts "invalid index\n"
        end

      _ ->
        IO.puts "wat\n"
    end

    loop
  end

  def prompt do
    IO.gets("debugger> ") 
    |> String.strip 
    |> String.split
  end
end
