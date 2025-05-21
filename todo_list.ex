defmodule Todo do
  defstruct [:text, complete?: false]
end

defmodule TodoList do
  @moduledoc """
  Todo lists seem list the default UI Hello World.
  Let's try it with a regular GenServer.
  """
  use GenServer

  # Client API

  def start do
    default_todo = %Todo{text: "Make a TODO list"}
    GenServer.start_link(__MODULE__, [default_todo])
  end

  def all(pid) do
    GenServer.call(pid, :all)
  end

  def add(pid, text) when is_pid(pid) and is_binary(text) do
    GenServer.cast(pid, {:add, text})
  end

  def complete(pid, index, complete?)
      when is_pid(pid) and is_integer(index) and is_boolean(complete?) do
    GenServer.cast(pid, {:complete, index, complete?})
  end

  def remove(pid, index) when is_pid(pid) and is_integer(index) do
    GenServer.cast(pid, {:remove, index})
  end

  def reword(pid, index, text) when is_pid(pid) and is_integer(index) and is_binary(text) do
    GenServer.cast(pid, {:reword, index, text})
  end

  def clear(pid) do
    GenServer.cast(pid, :clear)
  end

  # Server callbacks

  @impl true
  def init(state) do
    {:ok, state}
  end

  @impl true
  def handle_call(:all, state) do
    {:reply, state}
  end

  @impl true
  def handle_cast({:add, text}, state) do
    {:noreply, [%Todo{text: text} | state]}
  end

  @impl true
  def handle_cast({:complete, index, complete?}, state) do
    {:noreply, List.update_at(state, index, &%{&1 | complete?: complete?})}
  end

  @impl true
  def handle_cast({:remove, index}, state) do
    {:noreply, List.delete_at(state, index)}
  end

  @impl true
  def handle_cast({:reword, index, text}, state) do
    {:noreply, List.update_at(state, index, &%{&1 | text: text})}
  end

  @impl true
  def handle_cast({:clear}, state) do
    {:noreply, []}
  end
end
