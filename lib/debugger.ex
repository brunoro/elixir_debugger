defmodule Debugger do
  alias Debugger.Runner
  alias Debugger.Coordinator
  alias Debugger.PIDTable

  defmacro defdebug(header, do: body) do
    # TODO: binding retrieved via __CALLER__ had all variables as nil
    { _, _, quoted_params } = header
    vars = Enum.map quoted_params || [], fn({ var, meta, module }) ->
      { var, ({ var, meta, module }) }
    end

    quote do
      def unquote(header) do
        # TODO: only one PIDTable instance is running
        PIDTable.start_link

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
