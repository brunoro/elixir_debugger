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
        PIDTable.start_link

        binding = unquote(vars)
        scope = :elixir_scope.to_erl_env(__ENV__)

        ret = PIDTable.put(self, binding, scope)

        return_value = Runner.next(unquote(Macro.escape(body)))

        PIDTable.delete(self)
        return_value
      end
    end
  end
end 
