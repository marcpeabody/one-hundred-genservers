defmodule AlvesSupervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    children = [
      worker(Registry, [[name: MyApp, keys: :unique]]),
      worker(AlvesManager, [])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
