defmodule LinkCacheSupervisor do
  @moduledoc """
  Taken from https://thoughtbot.com/blog/make-phoenix-even-faster-with-a-genserver-backed-key-value-store
  """
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    children = [
      worker(LinkCacheCache, [[name: LinkCacheCache]])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
