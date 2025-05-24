defmodule MovieDataAgent do
  @moduledoc """
  Taken from https://elixircasts.io/intro-to-agents
  """

  def start_link do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def add(movie) do
    Agent.update(__MODULE__, fn state ->
      Map.put(state, movie, 1)
    end)
  end

  def reset do
    Agent.update(__MODULE__, fn _state -> %{} end)
  end

  def watch_count(movie) do
    Agent.get(__MODULE__, fn state ->
      Map.get(state, movie)
    end)
  end

  def watch(movie) do
    Agent.update(__MODULE__, fn state ->
      count = Map.get(state, movie)
      Map.replace(state, movie, count + 1)
    end)
  end
end
