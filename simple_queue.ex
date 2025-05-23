defmodule SimpleQueue do
  @moduledoc """
  Taken from https://elixirschool.com/en/lessons/advanced/otp_concurrency

  Personal note: FIFO might be a good choice for :queue - I think
  I've always done the inefficient 'list ++ [another]' to add to
  the end of a list like this but maybe I should stop.
  """
  use GenServer

  ### GenServer API

  @doc """
  GenServer.init/1 callback
  """
  # no @impl this time
  def init(state), do: {:ok, state}

  @doc """
  GenServer.handle_call/3 callback
  """
  def handle_call(:dequeue, _from, [value | state]) do
    {:reply, value, state}
  end

  def handle_call(:dequeue, _from, []), do: {:reply, nil, []}

  def handle_call(:queue, _from, state), do: {:reply, state, state}

  @doc """
  GenServer.handle_cast/2 callback
  """
  def handle_cast({:enqueue, value}, state) do
    {:noreply, state ++ [value]}
  end

  ### Client API / Helper functions

  def start_link(state \\ []) do
    GenServer.start_link(__MODULE__, state, name: __MODULE__)
  end

  def queue, do: Genserver.call(__MODULE__, :queue)
  def enqueue(value), do: GenServer.cast(__MODULE__, {:enqueue, value})
  def dequeue, do: GenServer.call(__MODULE__, :dequeue)
end
