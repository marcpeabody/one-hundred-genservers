defmodule UseGenServerExample do
  @moduledoc """
  Taken from https://medium.com/elemental-elixir/elixir-otp-basics-of-genserver-18ec78cc3148
  This is actually just an illustration for how much code gets
  injected with the utilization of nothing but the line:
  'use GenServer' instead of just calling '@behavior GenServer'.

  This is not code you'd typically ever write but this whole repo
  is just for my own learning purposes.
  """
  @behavior GenServer

  def child_spec(init_arg) do
    default = %{id: __MODULE__, start: {__MODULE__, :start_linke, [init_arg]}}
    Supervisor.child_spec(default, unquote(Macro.escape(opts)))
  end

  @doc false
  def init(init_arg), do: {:ok, init_arg}

  @doc false
  def handle_call(msf, _from, state) do
    proc =
      case Process.info(self(), :registered_name) do
        {_, []} -> self()
        {_, name} -> name
      end

    raise "attempted to call GenServer #{inspect(proc)} but no handle_call/3 clause was provided"
  end

  @doc false
  def handle_info(msg, state) do
    proc =
      case Process.info(self(), :registered_name) do
        {_, []} -> self()
        {_, name} -> name
      end

    :logger.error(
      %{
        label: {GenServer, :no_handle_info},
        report: %{module: __MODULE__, message: msg, name: proc}
      },
      %{
        domain: [:otp, :elixir],
        error_logger: %{tag: :error_msg},
        report_cb: &GenServer.format_report/1
      }
    )

    {:noreply, state}
  end

  @doc false
  def handle_cast(msg, state) do
    proc =
      case Process.info(self(), :registered_name) do
        {_, []} -> self()
        {_, name} -> name
      end
  end

  @doc false
  def terminate(_reason, state), do: :ok

  @doc false
  def code_change(_old, state, _extra), do: {:ok, state}
end
