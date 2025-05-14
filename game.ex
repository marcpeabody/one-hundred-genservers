defmodule Game do
  @moduledoc """
  Taken from https://curiosum.com/blog/what-is-elixir-genserver
  """
  use GenServer

  def start_link(state \\ []) do
    GenServer.start_link(__MODULE__, state)
  end

  def join(pid, player_id) do
    GenServer.cast(pid, {:join, player_id})
  end

  def get_players(pid) do
    GenServer.call(pid, :get_players)
  end

  def remove_player(pid, player_id) do
    # Why not use the GenServer API???
    send(pid, {:remove_player, player_id})
  end

  # no @impl
  def init(state) do
    {:ok, %{players: state}, {:continue, :prepare_game}}
  end

  def handle_cast({:join, player_id}, state) do
    updated_state = %{state | players: [player_id | state.players]}
    {:noreply, updated_state}
  end

  def handle_call(:get_players, _from, state) do
    {:reply, state.players, state}
  end

  def handle_info({:remove_player, player_id}, state) do
    updated_state = %{state | players: List.delete(state.players, player_id)}
    {:noreply, updated_state}
  end

  # this is new to me - yay for learning
  # odd that the trigger for this is :sys.change_code (words swapped)
  def code_change(_old_vsn, state, _extra) do
    {:ok, %{players: state}}
  end
end
