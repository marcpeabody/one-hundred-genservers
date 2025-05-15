defmodule AutoId do
  @moduledoc """
  Taken from https://dev.to/manhvanvu/elixir-agent-module-a-simple-way-to-sharing-data-between-processes-without-implement-our-process-or-genserver-5b5k
  """

  use Agent

  def start_link(initial_id) do
    Agent.start_link(fn -> initial_id end, name: __MODULE__)
  end

  def last_id do
    Agent.get(__MODULE__, & &1)
  end

  def set_id(new_id) do
    Agent.update(__MODULE__, fn _old -> new_id end)
  end

  def new_id do
    Agent.get_and_update(__MODULE__, fn id -> {id, id + 1} end)
  end
end
