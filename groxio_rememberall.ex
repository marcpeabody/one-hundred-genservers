defmodule GroxioRememberall do
  @moduledoc """
  Lesson introduces send/handle_info
  Taken from Groxio 4. OTP V2 GenServer Message Callbacks
  https://www.youtube.com/watch?v=Kifo000MSDo&t=3s
  """
  use GenServer

  # client

  def start_link(initial_value) do
    GenServer.start_link(__MODULE__, initial_value, name: :mem)
  end

  def get(pid \\ :mem) do
    GenServer.call(pid, :get)
  end

  def set(pid \\ :mem, value) do
    GenServer.cast(pid, {:set, value})
  end

  def say(pid \\ :mem) do
    send(pid, :say)
  end

  # callback

  @impl true
  def init(stack) do
    {:ok, stack}
  end

  @impl true
  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_cast({:set, value}, _state) do
    {:noreply, value}
  end

  @impl true
  def handle_info(:say, state) do
    IO.puts("The value is #{state}")
    {:noreply, state}
  end
end
