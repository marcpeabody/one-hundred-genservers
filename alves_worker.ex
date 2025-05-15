defmodule AlvesWorker do
  use GenServer
  use Timex

  @initial_score 1_000
  @score_reducer 60
  @event_threshol 2000
  @dismiss_threshold 1

  def start_link(name, group) do
    with {:ok, pid} <-
           GenServer.start_link(
             __MODULE__,
             %{score: @initial_score, group: group, initial_time: group.inserted_at},
             name: name
           ) do
      # an empty with?!?
    end
  end

  def init(state) do
    # Schedule work to be performed on start
    schedule_reporter()
    {:ok, state}
  end

  # CLIENT API

  def new_message(message) do
    if message.group_id do
      pid = lookup(message.group_id)

      if pid do
        GenServer.cast(pid, {:new_message, message.user_id})
      end
    end
  end

  def current_score(group_id) do
    pid = lookup(group_id)
    GenServer.call(pid, {:current_score})
  end

  # SERVER API

  def handle_info(:report, state) do
    new_state = decay_points(state)

    cond do
      new_state.score >= @event_threshold ->
        with {:ok, _group} <- Groups.notify_group_event(state.group.id) do
          AlvesManager.dismiss_worker(state.group.id)
          # exists worker
          terminate_process(state.group.id)
        end

      new_state.score <= @dismiss_threshold ->
        with {:ok, _group} <- Groups.delete_group(state.group) do
          # Broadcast termination to clients
          Enum.each(state.group.user_ids, fn user_id ->
            UserBroadcaster.group_removed(user_id, state.group.id)
          end)

          Manager.dismiss_worker(state.group.id)
          terminate_process(state.group.id)
        else
          err ->
            IO.inspect(err)
        end

      true ->
        # Broadcast current score to clients
        Enum.each(state.group.user_ids, fn user_id ->
          UserBroadcaster.group_score(user_id, state.group.id, state.score)
        end)

        # Reschedule once more
        schedule_reporter()
    end

    {:noreply, new_state}
  end

  defp terminate_process(group_id) do
    group_id
    |> lookup()
    |> Process.exit(:normal)
  end

  def handle_cast({:new_message, user_id}, state) do
    # Determines the new score based on previous messages AND the party (some code omitted)
    msg_score = msg_score_decay()

    # other fields left out
    new_state = %{state | score: Kernel.trunc(state.score + msg_score)}

    {:noreply, new_state}
  end

  def handle_call({:current_score}, _from, state) do
    {:reply, state.score, state}
  end

  # AUX FUNCTIONS

  defp decay_points(%{score: score, initial_time: time} = state) do
    current_time = Timex.now()
    diff_in_s = Timex.diff(current_time, time, :seconds)

    final_score =
      (score * :math.pow(0.95, diff_in_s / @score_reducer)) |> Kernel.trunc()

    Map.put(state, :score, final_score)
  end

  defp schedule_reporter() do
    # 2 seconds
    Process.send_after(self(), :report, 2 * 1000)
  end

  defp msg_score_decay() do
    # not implemented
  end

  defp lookup(group_id) do
    case Registry.lookup(Registry.AlvesApp, "group-#{group_id}") do
      [{pid, _}] -> pid
      _ -> nil
    end
  end
end
