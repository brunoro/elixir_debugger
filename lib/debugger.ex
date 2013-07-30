defmodule Debugger do
  alias Debugger.Runner
  alias Debugger.Coordinator

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

        { :ok, pid } = Coordinator.spawn(binding, scope)
        return_value = Runner.next(pid, unquote(Macro.escape(body)))

        Coordinator.done(pid)
        return_value
      end
    end
  end
end 
