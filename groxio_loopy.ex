defmodule GroxioLoopy do
  @moduledoc """
  Loop loop loop loop.
  Taken from Groxio 2. OTP V2 Behaviors
  https://www.youtube.com/watch?v=lyOzLyb4lVA
  The Dog on the Floor is an Elixir Guru.
  """
  @callback work() :: :ok
  @callback wait() :: :ok

  def run(module) do
    spawn(&loop(module))
  end

  def loop(module) do
    module
    |> work()
    |> loop()
  end

  def work(module) do
    module.wait()
    module.work()
    module
  end
end
