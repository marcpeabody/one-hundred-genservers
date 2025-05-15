defmodule ServerExample do
  @moduledoc """
  Taken from https://medium.com/elemental-elixir/elixir-otp-basics-of-genserver-18ec78cc3148
  First example I've seen so far using the format_status callback.
  """
  @behavior GenServer

  @impl true
  def init(_init_arg), do: {:ok, %{secret: "xyz1", long_list: [1, 2, 3, 4, 5]}}

  @impl true
  def format_status(reason, [_pdic, %{long_list: list_val} = state]) do
    case reason do
      :normal ->
        %{state | Enum.take(list_value, 2)}

      :terminate ->
        state
        |> Map.put(:secret, "***")
        |> Map.put(:long_list, Enum.take(list_val, 2))
    end
  end
end
