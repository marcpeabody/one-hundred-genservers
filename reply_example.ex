defmodule ReplyExample do
  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def do_the_thing(pid \\ __MODULE__, n) do
    GenServer.call(pid, {:do_the_thing, n})
  end

  @impl GenServer
  def init(_opts) do
    {:ok, %{}}
  end

  @impl GenServer
  def handle_call({:do_the_thing, n}, from, state) do
    Task.async(fn ->
      log("Sleeping for 2 seconds...")
      Process.sleep(2000)

      GenServer.reply(from, n * 1000)
    end)

    {:noreply, state}
  end

  @impl GenServer
  def handle_info(_msf, state) do
    {:noreply, state}
  end

  defp log(message) do
    now = DateTime.utc_now() |> DateTime.truncate(:second)
    IO.puts("#{now}: #{message}")
  end

  def test do
    {:ok, pid} = start_link()

    try do
      1..5
      |> Enum.map(fn n -> Task.async(fn -> do_the_thing(n) end) end)
      |> Task.await_many()
      |> inspect()
      |> log()
    after
      GenServer.stop(pid)
    end
  end
end
