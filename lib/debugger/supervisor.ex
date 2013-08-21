defmodule Debugger.Supervisor do
  alias Debugger.PIDTable
  use Supervisor.Behaviour

  def start_link do
    IO.puts "Debugger #{inspect self}: Supervisor.start_link"
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    children = [
      # Define workers and child supervisors to be supervised
      worker(Debugger.PIDTable, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end
end
