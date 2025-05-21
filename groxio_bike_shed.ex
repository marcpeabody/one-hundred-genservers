defmodule GroxioBikeShed do
  use GenServer

  # client

  def start_link(input) do
    GenServer.start_link(__MODULE__, input, name: :shed)
  end

  def show(history_id \\ :shed) do
    GenServer.call(history_id, {:show})
    |> IO.puts()
  end

  def add(history_id \\ :shed, item) do
    GenServer.cast({:add, item})
  end

  # callbacks

  @impl GenServer
  def init(input) do
    {:ok, GroxioBikeShedCore.new(input)}
  end

  @impl GenServer
  def handle_call(:show, _from, history) do
    {:reply, GroxioBikeShedCore.show(history), history}
  end

  @impl GenServer
  def handle_cast({:add, item}, history) do
    {:noreply, GroxioBikeShedCore.add(history, item)}
  end
end
