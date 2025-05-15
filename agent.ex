defmodule Agent do
  @moduledoc """
  Literally the Agent implementation but without all the docs.

  Taken from https://github.com/elixir-lang/elixir/blob/main/lib/elixir/lib/agent.ex
  """

  @type on_start :: {:ok, pid} | {:error, {:already_started, pid} | term}

  @type name :: atom | {:global, term} | {:via, module, term}

  @type agent :: pid | {atom, node} | name

  @type state :: term

  def child_spec(arg) do
    %{
      id: Agent,
      start: {Agent, :start_link, [arg]}
    }
  end

  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      if not Module.has_attribute?(__MODULE__, :doc) do
        @doc """
        Returns a specification to start this module under a supervisor.

        See `Supervisor`.
        """
      end

      def child_spec(arg) do
        default = %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [arg]}
        }

        Supervisor.child_spec(default, unquote(Macro.escape(opts)))
      end
    end

    defoverridable child_spec: 1
  end

  @spec start_link((-> term), GenServer.options()) :: on_start
  def start_link(fun, options \\ []) when is_function(fun, 0) do
    GenServer.start_link(Agent.Server, fun, options)
  end

  def start_link(module, fun, args, options \\ []) do
    GenServer.start_link(Agent.Server, {module, fun, args}, options)
  end

  @spec start((-> term), GenServer.options()) :: on_start
  def start(fun, options \\ []) when is_function(fun, 0) do
    GenServer.start(Agent.Server, fun, options)
  end

  @spec start(module, atom, [term], GenServer.options()) :: on_start
  def start(module, fun, args, options \\ []) do
    GenServer.start(Agent.Server, {module, fun, args}, options)
  end

  @spec get(agent, (state -> a), timeout) :: a when a: var
  def get(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:get, fun}, timeout)
  end

  @spec get(agent, module, atom, [term], timeout) :: term
  def get(agent, module, fun, args, timeout \\ 5000) do
    GenServer.call(agent, {:get, {module, fun, args}}, timeout)
  end

  @spec get_and_update(agent, (state -> {a, state}), timeout) :: a when a: var
  def get_and_update(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:get_and_update, fun}, timeout)
  end

  @spec get_and_update(agent, module, atom, [term], timeout) :: term
  def get_and_update(agent, module, fun, args, timeout \\ 5000) do
    GenServer.call(agent, {:get_and_update, {module, fun, args}}, timeout)
  end

  @spec update(agent, (state -> state), timeout) :: :ok
  def update(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:update, fun}, timeout)
  end

  @spec update(agent, module, atom, [term], timeout) :: :ok
  def update(agent, module, fun, args, timeout \\ 5000) do
    GenServer.call(agent, {:update, {module, fun, args}}, timeout)
  end

  @spec cast(agent, (state -> state)) :: :ok
  def cast(agent, fun) when is_function(fun, 1) do
    GenServer.cast(agent, {:cast, fun})
  end

  @spec cast(agent, module, atom, [term]) :: :ok
  def cast(agent, module, fun, args) do
    GenServer.cast(agent, {:cast, {module, fun, args}})
  end

  @spec stop(agent, reason :: term, timeout) :: :ok
  def stop(agent, reason \\ :normal, timeout \\ :infinity) do
    GenServer.stop(agent, reason, timeout)
  end
end
