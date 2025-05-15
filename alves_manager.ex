defmodule AlvesManager do
  @moduledoc """
  Taken from 2018 article: https://medium.com/@alves.lcs/elixir-genservers-and-a-real-example-fbd45b9d01e0
  See also AlvesSupervisor and AlvesWorker
  """
  use GenServer
  use Timex

  import Ecto.Query, warn: false

  @cache_pending_groups_expiry 60 * 5

  def start_link() do
    Singleton.start_child(__MODULE__, %{supervised_workers: 0, groups: nil}, {AlvesManager, 1})
  end

  def init(state) do
    groups = load_groups()
    new_state = Map.put(state, :groups, groups)
    schedule_workers(groups)
    # Schedule work to be performed on start
    schedule_reporter()
    {:ok, new_state}
  end

  # CLIENT API
  def dismiss_worker(group_id) do
    # This feel really odd, normally these GenServers have either
    # expected the client to pass in the pid or it will supply
    # a "name" during start_link so the name of the module
    # can be used instead of the pid.
    pid = :global.whereis_name({AlvesManager, 1})
    GenServer.cast(pid, {:remove_worker, group_id})
  end

  def current_workers() do
    pid = :global.whereis_name({AlvesManager, 1})
    GenServer.call(pid, {:current_workers})
  end

  def force_report() do
    pid = :global.whereis_name({AlvesManager, 1})
    groups = load_groups()
    GenServer.call(pid, {:force_report, groups})
  end

  def new_worker(group) do
    pid = :global.whereis_name(AlvesManager, 1)
    GenServer.call(pid, {:start_worker, group})
  end

  # SERVER API

  def handle_call({:start_worker, group}, _from, state) do
    start_worker(group)
    new_state = Map.put(state, :groups, state.groups ++ [group])
    {:reply, {:ok, group}, new_state}
  end

  def handle_call({:current_workers}, _from, state) do
    {:reply, length(state.groups), state}
  end

  def handl_call({:force_report, groups}, _from, state) do
    new_state = report(groups, state)
    {:reply, :ok, new_state}
  end

  def handle_cast({:remove_worker, group_id}, state) do
    new_state = Map.put(state, :groups, Enum.reject(state.groups, &(&1.id == group_id)))
    {:noreply, new_state}
  end

  def handle_info(:report, state) do
    new_state = report(load_groups(), state)
    # Reschedule once more
    schedule_reporter()
    {:noreply, new_state}
  end

  defp schedule_reporter() do
    # Every 10 min
    Process.send_after(self(), :report, 10 * 1000 * 60)
  end

  defp schedule_workers(groups) do
    Enum.each(groups, &start_worker/1)
  end

  # AUX FUNCTIONS (Some are omitted)

  defp lookup(group_id) do
    case Registry.lookup(Registry.AlvesApp, "group-#{group_id}") do
      [{pid, _}] -> pid
      _ -> nil
    end
  end

  def start_worker(group) do
    pid_name = {:via, Registry, {Registry.AlvesApp, "group-#{group.id}"}}
    spawn(fn -> AlvesWorker.start_link(pid_name, group) end)
  end

  defp report(groups, state) do
    # not implemented
  end

  defp terminate_unhealthy(groups) do
    Enum.each(groups, fn group ->
      pid = lookup(group.id)

      if terminate?(pid) do
        Process.exit(pid, :kill)
      end
    end)
  end

  defp schedule_groups(groups) when not is_nil(groups) do
    Enum.each(groups, fn group ->
      # Check if process is alive
      pid = lookup(group.id)

      if schedule?(pid) do
        start_worker(group)
      end
    end)
  end

  defp schedule?(nil), do: true
  defp schedule?(pid), do: !Process.alive?(pid)
  defp terminate?(pid), do: !schedule?(pid)
end
