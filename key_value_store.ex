defmodule KeyValueStore do
  @moduledoc """
  Taken from https://medium.com/elemental-elixir/elixir-otp-basics-of-genserver-18ec78cc3148
  """
  @behavior GenServer

  # client api
  def start() do
    GenServer.whereis(__MODULE__) ||
      GenServer.start(__MODULE__, %{}, name: __MODULE__)

    :ok
  end

  def get(key), do: GenServer.call(__MODULE__, {:get, key})

  def put(key, val), do: GenServer.cast(__MODULE__, {:put, key, val})

  def delete(key), do: GenServer.cast(__MODULE__, {:delete, key})

  # server api
  @impl true
  def init(init_state) when is_map(init_state), do: {:ok, init_state}
  # this is... very forgiving
  def init(_), do: {:ok, %{}}

  @impl true
  def handle_call({:get, key}, _, state), do: {:reply, state[key], state}
  def handle_call(_, _, state), do: {:reply, :invalid_request, state}

  @impl true
  def handle_cast({:put, key, val}, state) do
    {:noreply, Map.put(state, key, val)}
  end

  def handle_cast({:delete, key}, state) do
    {:noreply, Map.delete(state, key)}
  end

  def handle_cast(_, state), do: {:noreply, state}
end
