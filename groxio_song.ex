defmodule GroxioSong do
  @behaviour GroxioLoopy

  def go do
    Loopy.run(__MODULE__)
  end

  @impl GroxioLoopy
  def work do
    IO.puts("""
    Louie, Louie,
    Oh no, we gotta go
    yeah yeah yeah yeah

    """)
  end

  @impl GroxioLoopy
  def wait do
    Process.sleep(100)
  end
end
