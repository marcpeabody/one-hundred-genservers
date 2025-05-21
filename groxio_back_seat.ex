defmodule GroxioBackSeat do
  @behaviour GroxioLoopy

  def go do
    Loopy.run(__MODULE__)
  end

  @impl GroxioLoopy
  def work do
    [
      "ow\n",
      "quit it\n",
      "are we there yet\n",
      "let's go to rock city\n",
      "i'm hungry\n"
    ]
    |> Enum.random()
    |> IO.puts()
  end

  @impl GroxioLoopy
  def wait do
    5_000 |> :random.uniform() |> Process.sleep()
  end
end
