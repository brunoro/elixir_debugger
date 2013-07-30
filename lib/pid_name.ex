defmodule Debugger.PIDName do
  defp pid_name_prefix, do: "__PID_"

  # #PID<0.49.0> -> :"__PID_0_49_0__"
  def pid_name(pid) do
    esc_pid = pid |> pid_to_list 
                  |> to_binary 
                  |> String.lstrip("<")
                  |> String.rstrip(">")
                  |> String.replace(".", "_") 

    "#{pid_name_prefix}#{esc_pid}__"
  end

  def is_pid_name?(bin), do: String.starts_with?(bin, pid_name_prefix)
end
