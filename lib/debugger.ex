defmodule Debugger do
  alias Debugger.Runner
  alias Debugger.PIDTable
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    Debugger.Supervisor.start_link
  end

  defmacro defdebug(header, do: body) do
    # TODO: binding retrieved via __CALLER__ had all variables as nil
    { _, _, quoted_params } = header
    vars = Enum.map quoted_params || [], fn({ var, meta, module }) ->
      { var, ({ var, meta, module }) }
    end

    quote do
      def unquote(header) do
        binding = unquote(vars)
        scope = :elixir_scope.to_erl_env(__ENV__)
        PIDTable.start(self, binding, scope)

        result = Runner.next(unquote(Macro.escape(body)))

        PIDTable.finish(self)
        
        case result do
          { :ok, value } ->
            value
          { :exception, kind, reason, stacktrace } ->
            :erlang.raise(kind, reason, stacktrace)
        end
      end
    end
  end
end 
