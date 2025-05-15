defmodule M do
  @moduledoc """
  Taken from https://elixirforum.com/t/looking-for-clarity-around-using-agent/4750/3

  Here a GenServer is used to illstrate OP's Agent code as a GenServer

  Inciteful comment from peerreynders:
  Agents are often used to introduce the concept of processes because
  the code looks initially much less arcane than GenServer code.
  """
  use GenServer

  # instead of fn -> [1,2,3] end
  def create_object() do
    {:ok, pid} = GenServer.start_link(__MODULE__, [1, 2, 3])
    pid
  end

  def update_object(pid, new_data) do
    GenServer.call(pid, {:update, new_data})
    pid
  end

  def get_object(pid) do
    IO.inspect(GenServer.call(pid, :get))
    pid
  end

  # GenServer callbacks
  def init(state) do
    # TODO initialization logic
    {:ok, state}
  end

  # instead of `fn (state) -> state ++ new_data end`
  def handle_call({:update, new_data}, _from, state) do
    {:reply, :ok, state ++ new_data}
  end

  # instead of `&(&1)`
  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end
end

# M.create_object() |> M.update_object([4,5]) |> M.get_object()
