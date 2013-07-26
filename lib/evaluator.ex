defmodule Evaluator do
  use GenServer.Behaviour

  defrecord State, [binding: nil, scope: nil, stack: []]
  defrecord Pid, list: nil

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
  
  def handle_cast(:pop_state, State[stack: [state]]), do: { :noreply, state }
  def handle_cast(:pop_state, State[stack: [old_state | _]]) do
    { :noreply, old_state }
  end

  def handle_cast(:push_state, state) do
    { :noreply, state.stack([state | state.stack]) }
  end

  # calls
  def handle_call({ :eval, expr }, _from, state) do
    { value, new_state } = Evaluator.eval_quoted(expr, state)
    { :reply, value, new_state }
  end

  def handle_call({ :expand, expr }, _from, state) do
    { _, meta, _ } = expr
    ex_scope = :elixir_scope.to_ex_env({ meta[:line] || 0, state.scope })
    expanded = Macro.expand_once(expr, ex_scope)
 
    { :reply, expanded, state }
  end

  def handle_call(:get_state, _from, state) do
    { :reply, state, state }
  end

  def handle_call({ :match, { value, clauses }}, _from, state) do
    { matching, new_state } = 
      Evaluator.find_matching_clause(value, clauses, state)

    # TODO: is this right? scope should be specific to match operation
    { :reply, matching, new_state }
  end

  def handle_call(:receive, _from, state) do
    receive_code = quote do
      receive do
        value -> value
      end
    end
    Evaluator.eval_quoted(receive_code, state)
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

    """
    { :case, __META__,
      [{:value, [], Elixir},
       [do: all_clauses ]] }
    """
    match_clause_case = quote do
      case unquote(value) do
        unquote(all_clauses)
      end
    end
    
    Evaluator.eval_quoted(match_clause_case, state)
  end

  # TODO: use scope on eval?
  def eval_quoted(expr, state) do
    { value, binding, scope } = :elixir.eval_quoted([expr], state.binding)
    { wrap_pid(value), state.binding(binding).scope(scope) }
  end

  # no pids shall pass
  def wrap_pid(pid) when is_pid(pid), do: Pid[list: pid_to_list(pid)]
  def wrap_pid(value),                do: value

  # client functions
  def done(pid),       do: :gen_server.cast(pid, :done)
  def pop_state(pid),  do: :gen_server.cast(pid, :pop_state)
  def push_state(pid), do: :gen_server.cast(pid, :push_state)

  def eval(pid, expr),            do: :gen_server.call(pid, { :eval, expr })
  def expand(pid, expr),          do: :gen_server.call(pid, { :expand, expr })
  def get_state(pid),             do: :gen_server.call(pid, :get_state)
  def match(pid, value, clauses), do: :gen_server.call(pid, { :match, { value, clauses }})
  def receive_value(pid),         do: :gen_server.call(pid, :receive)

  # Makes nested Evaluator.next calls until leafs are reached.
  # Evaluates leaf expressions by sending them to pid, which
  # keeps the current scope and binding

  # expansions that lead to case-like expressions should be kept
  defp do_or_expand(pid, expr, fun) do 
    expanded = Evaluator.expand(pid, expr)
    case expanded do
      { :case, _, _ } ->
        Evaluator.next(pid, expanded)
      _ ->
        fun.()
    end
  end

  # discard state changes made by evals ran by fun/0
  defp do_on_new_state(pid, fun) do
    # enter a new state level
    Evaluator.push_state(pid)
    return = fun.()
    Evaluator.pop_state(pid)
    return
  end

  # values shouldn't be evaluated
  def next(_, value) when is_number(value), do: value
  def next(_, value) when is_binary(value), do: value
  def next(_, value) when is_atom(value),   do: value
  
  def next(_, pid) when is_pid(pid), do: Pid[list: pid_to_list(pid)]
  
  # case
  def next(pid, { :case, _, [condition | [do: clauses]] }) do
    condition_value = Evaluator.next(pid, condition)
    Evaluator.match_next(pid, condition_value, clauses) # is there more than do?
  end

  # receive
  def next(pid, { :receive, _, [[do: clauses]] }) do
    received_value = Evaluator.receive_value(pid)
    Evaluator.match_next(pid, received_value, clauses) 
  end

  # anonymous functions
  def next(pid, { :fn, _, [[do: clauses]] }) do
    received_value = Evaluator.receive_value(pid)
    Evaluator.match_next(pid, received_value, clauses) 
  end

  # spawn should spawn another evaluator as well
  def next(pid, { :spawn, _, [fun] }) do
    state = Evaluator.get_state(pid)
    { :ok, evaluator_pid } = :gen_server.start_link(Evaluator, state, [])

    spawn fn ->
      Evaluator.next(evaluator_pid, fun)
      Evaluator.done(evaluator_pid)
    end

    # pids are stored as binaries (because of `invalid quoted expression: #PID<..>`)
    Pid[list: pid_to_list(evaluator_pid)]
  end

  # sending messages
  # TODO: all Process & Kernel methods taking pids as arguments should be reimplemented
  def next(pid, { :<-, _, [dest_pid, msg] }) do
    msg_val  = Evaluator.next(pid, msg)
    Pid[list: list] = Evaluator.next(pid, dest_pid)

    list_to_pid(list) <- msg_val
  end

  # assignments
  def next(pid, { :=, meta, [left | [right]] }) do
    right_value = Evaluator.next(pid, right)
    Evaluator.eval(pid, { :=, meta, [left | [right_value]] })
  end

  # list of expressions
  def next(pid, { type, meta, expr_list }) when is_list(expr_list) do
    expr = { type, meta, expr_list }

    do_or_expand pid, expr, fn ->
      value_list = Enum.map(expr_list, Evaluator.next(pid, &1))
      Evaluator.eval(pid, { type, meta, value_list })
    end
  end

  # other expressions are evaluated directly
  def next(pid, expr) do
    do_or_expand pid, expr, fn ->
      Evaluator.eval(pid, expr)
    end
  end

  # pattern matching operator should evaluate clauses until
  # the first clause matching the condition is found
  def match_next(pid, value, { :-> , _, clauses }) do
    do_on_new_state pid, fn ->
      matching_clause = Evaluator.match(pid, value, clauses)
      { _, _, right } = matching_clause
      Evaluator.next(pid, right)
    end
  end
end
