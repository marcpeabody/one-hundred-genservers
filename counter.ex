defmodule Counter do
  @moduledoc """
  Taken from https://medium.com/elemental-elixir/elixir-otp-basics-of-genserver-18ec78cc3148
  This tutorial walks you through the actual process
  of updating code and redefining live through the use of code_change.

  Only copying the "updated" version here but this tutorial
  is a ridiculously great and thorough tutorial for learning!
  """
  @vsn "2"
  @behavior GenServer
  @impl true
  def init(_), do: {:ok, %{count: 0}}
  @impl true
  def handle_call(:increment, _, %{count: count}) do
    {:reply, count + 1, %{count: count + 1}}
  end

  @impl true
  def code_change(old_vsn, state, extra) do
    IO.puts("code change - #{old_vsn}, #{state}, #{extra}")
    {:ok, %{count: state}}
  end
end
