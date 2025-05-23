defmodule AgentServer do
  @moduledoc """
  Actually Agent.Server
  The GenServer server side is completely in a separate module from Agent.

  Taken from https://github.com/elixir-lang/elixir/blob/main/lib/elixir/lib/agent/server.ex
  """

  use GenServer

  def init(fun) do
    _ = initial_call(fun)
    {:ok, run(fun, [])}
  end

  def handle_call({:get, fun}, _from, state) do
    {:reply, run(fun, [state]), state}
  end

  def handle_call({:get_and_update, fun}, _from, state) do
    case run(fun, [state]) do
      {reply, state} -> {:reply, reply, state}
      other -> {:stop, {:bad_return_value, other}, state}
    end
  end

  def handle_call({:update, fun}, _from, state) do
    {:reply, :ok, run(fun, [state])}
  end

  def handle_cast({:cast, fun}, state) do
    {:noreply, run(fun, [state])}
  end

  def code_change(_old, state, fun) do
    {:ok, run(fun, [state])}
  end

  defp initial_call(mfa) do
    _ = Process.put(:"$initial_call", get_initial_call(mfa))
    :ok
  end

  defp get_initial_call(fun) when is_function(fun, 0) do
    {:module, module} = Function.info(fun, :module)
    {:name, name} = Function.info(fun, :name)
    {module, name, 0}
  end

  defp get_initial_call({mod, fun, args}) do
    {mod, fun, length(args)}
  end

  defp run({m, f, a}, extra), do: apply(m, f, extra ++ a)
  defp run(fun, extra), do: apply(fun, extra)
end
