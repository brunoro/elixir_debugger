defmodule Evaluator do
  use GenServer.Behaviour

  defrecord State, [binding: nil, scope: nil, stack: []]

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

        { :ok, pid }
          = :gen_server.start_link(Evaluator, State[binding: binding, scope: scope], [])
        return_value = Evaluator.next(pid, unquote(Macro.escape(body)))

        Evaluator.done(pid)
        return_value
      end
    end
  end
 
  def init(state) do
    scope = :elixir_scope.vars_from_binding(state.scope, state.binding)
    { :ok, state.scope(scope) }
  end
    
  # casts
  def handle_cast(:done, state) do
    { :stop, :normal, state }
  end
  
  def handle_cast(:pop_stack, State[stack: [state]]), do: { :noreply, state }
  def handle_cast(:pop_stack, State[stack: [old_state | _]]) do
    { :noreply, old_state }
  end

  def handle_cast(:push_stack, state) do
    { :noreply, state.stack([state | state.stack]) }
  end

  def handle_cast({ :put_state, new_state }, _state) do
    { :noreply, new_state }
  end

  # calls
  def handle_call(:get_state, _from, state) do
    { :reply, state, state }
  end

  # client functions
  def done(pid),             do: :gen_server.cast(pid, :done)
  def get_state(pid),        do: :gen_server.call(pid, :get_state)
  def put_state(pid, state), do: :gen_server.cast(pid, { :put_state, state })

  def pop_stack(pid),        do: :gen_server.cast(pid, :pop_stack)
  def push_stack(pid),       do: :gen_server.cast(pid, :push_stack)

  # how to evaluate expressions 
  # TODO: use scope on eval?
  def eval_quoted(expr, state) do
    { value, binding, scope } = :elixir.eval_change_state([expr], state.binding)
    wrap_pid(value, state.binding(binding).scope(scope))
  end

  # add pids to binding with some name mangling
  # NO PIDS SHALL PASS!!
  def wrap_pid(pid, state) when is_pid(pid) do 
    var = binary_to_atom(pid_name(pid))
    new_binding = Keyword.put(state.binding, var, pid)

    {{ var, [], Elixir }, state.binding(new_binding) }
  end
  def wrap_pid(value, state), do: { value, state }

  # #PID<0.49.0> -> :"__PID_0_49_0__"
  def pid_name(pid) do
    esc_pid = pid |> pid_to_list 
                  |> to_binary 
                  |> String.lstrip("<")
                  |> String.rstrip(">")
                  |> String.replace(".", "_") 

    "__PID_#{esc_pid}__"
  end

  # functions manipulating state
  def change_state(fun) do
    state = Evaluator.get_state
    { result, new_state } = fun(state)
    Evaluator.put_state(new_state)
  end

  def eval_change_state(expr) do
    change_state fn(state) ->
      Evaluator.eval_quoted(expr, state)
    end
  end

  def find_matching_clause(value, clauses, state) do 
    # generates `unquote(lhs) -> unquote(Macro.escape clause)`
    clause_list = Enum.map clauses, fn(clause) ->
      { left, _, _ } = clause
      esc_clause = Macro.escape clause

      { left, [], esc_clause }
    end

    # if no clause is matched return :nomatch
    nil_clause = {[{:_, [], Elixir}], [], :nomatch}
    all_clauses = { :->, [], List.concat(clause_list, [nil_clause]) }

    match_clause_case = quote do
      case unquote(value) do
        unquote(all_clauses)
      end
    end
    
    Evaluator.eval_quoted(match_clause_case, state)
  end

  def expand(expr) do
    state = Evaluator.get_state
    { _, meta, _ } = expr
    ex_scope = :elixir_scope.to_ex_env({ meta[:line] || 0, state.scope })
    Macro.expand_once(expr, ex_scope)
  end

  def do_receive() do
    state = Evaluator.get_state
    receive_code = quote do
      receive do
        value -> value
      end
    end
    Evaluator.eval_quoted(receive_code, state)
  end

  # expansions that lead to case-like expressions should be kept
  defp do_or_expand(expr, fun) do 
    expanded = Evaluator.expand(expr)
    case expanded do
      { :case, _, _ } ->
        Evaluator.next(expanded)
      _ ->
        fun.()
    end
  end

  # Makes nested Evaluator.next calls until leafs are reached.
  # Evaluates leaf expressions by sending them to pid, which
  # keeps the current scope and binding

  # values shouldn't be evaluated
  def next(_, value) when is_number(value), do: value
  def next(_, value) when is_binary(value), do: value
  def next(_, value) when is_atom(value),   do: value
  
  # variables sholdn't be next'd
  # TODO: functions with one argument would fall here too
  def next(_, { var, meta, mod }) when is_atom(var) and is_atom(mod) do
    { var, meta, mod }
  end

  # case
  def next({ :case, _, [condition | [do: clauses]] }) do
    condition_value = Evaluator.next(condition)
    Evaluator.match_next(condition_value, clauses) # is there more than do?
  end

  # receive
  def next({ :receive, _, [[do: clauses]] }) do
    received_value = Evaluator.do_receive(state)
    Evaluator.match_next(received_value, clauses) 
  end

  # assignments
  def next({ :=, meta, [left | [right]] }) do
    right_value = Evaluator.next(right)

    Evaluator.eval_change_state({ :=, meta, [left | [right_value]] }, state)
  end

  # list of expressions
  def next({ type, meta, expr_list }) when is_list(expr_list) do
    expr = { type, meta, expr_list }

    do_or_expand pid, expr, fn ->
      value_list = Enum.map(expr_list, Evaluator.next(&1))
      Evaluator.eval_change_state({ type, meta, value_list })
    end
  end

  # other expressions are evaluated directly
  def next(expr) do
    do_or_expand expr, fn ->
      Evaluator.eval_change_state(expr)
    end
  end

  # pattern matching operator should evaluate clauses until
  # the first clause matching the condition is found
  def match_next(pid, value, { :-> , _, clauses }) do
    { _, _, right } = change_state fn(state) ->
      Evaluator.find_matching_clause(value, clauses, state)
    end
    Evaluator.next(right)
  end
end
