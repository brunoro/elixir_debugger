defmodule Debugger.Escape do

  @pid_prefix "__PID_"
  @fun_prefix "__FUN_"

  def is_escaped?(bin) do
    String.starts_with?(bin, @pid_prefix) or
    String.starts_with?(bin, @fun_prefix)
  end

  # #PID<0.49.0> -> :__PID_0_49_0__
  def escape(pid) when is_pid(pid) do
    esc_pid = pid |> pid_to_list 
                  |> to_binary 
                  |> String.replace("<", "")
                  |> String.replace(">", "")
                  |> String.replace(".", "_") 

    "#{@pid_prefix}#{esc_pid}__"
  end

  # :"-expr/5-fun-2-" -> "__FUN_expr_5_fun_2__"
  def escape(fun) when is_function(fun) do
    fun_info = :erlang.fun_info(fun)
    fun_name = fun_info[:name] |> to_binary
                               |> String.strip("-")
                               |> String.replace("-", "_")
                               |> String.replace("/", "_")


    "#{@fun_prefix}#{fun_name}__"
  end
end
