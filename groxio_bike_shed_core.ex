defmodule GroxioBikeShedCore do
  @max_length 3

  def new() do
    String.split(input, ",", trim: true)
  end

  def add(history, item) do
    [item | history]
    |> Enum.take(@max_length)
  end

  def show(history) do
    Enum.join(history, "\n")
  end
end
