defmodule Periodically do
  @moduledoc """
  A second example from ttps://hexdocs.pm/elixir/1.12/GenServer.html
  See also Stack.
  """
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{})
  end

  @impl true
  def init(state) do
    # Schedule work to be performed on statrt
    schedule_work()

    {:ok, state}
  end

  @impl true
  def handle_info(:work, state) do
    # TODO

    schedule_work()

    {:noreply, state}
  end

  defp schedule_work do
    # We schedule the work to happen in 2 hours
    # (written in milliseconds)
    # Alternatively, one might write :timer.hours(2)
    Process.send_after(self(), :work, 2 * 60 * 60 * 1000)
  end
end
