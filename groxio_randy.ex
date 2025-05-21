defmodule GroxioRandy do
  @moduledoc """
  Random number generator as GenServer
  Taken from Groxio 1. OTP V2 OTP Basics
  https://www.youtube.com/watch?v=v438ONPryOE
  Bruce Tate is a cool dude.
  """
  use GenServer

  @impl true
  def init(max) do
    {:ok, max}
  end

  @impl true
  def handle_call(:pop, _from, max) do
    {:reply, :rand.uniform(max), max}
  end
end
