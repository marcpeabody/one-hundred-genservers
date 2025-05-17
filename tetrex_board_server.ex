defmodule TetrexBoardServer do
  @moduledoc """
  Taken from https://github.com/mjftw/Tetrex
  Multiplayer Tetris
  See the conference presentation: https://www.youtube.com/watch?v=87iFws4c78s
  """
  use GenServer

  @type init_args :: [height: non_neg_integer(), width: non_neg_integer(), random_seed: integer()]

  # Client API

  @spec start_link(init_args()) :: pid()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  @spec start(init_args()) :: pid()
  def start(opts \\ []) do
    GenServer.start(__MODULE__, [], opts)
  end

  @spec new(pid() | atom(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ::
          TetrexBoard.board_preview()
  def new(board_pid, height, width, seed) do
    GenServer.call(board_pid, {:new, height, width, seed})
  end

  @spec preview(pid()) :: TetrexBoard.board_preview()
  def preview(board_pid) do
    GenServer.call(board_pid, :preview)
  end

  @spec try_move_left(pid()) :: {TetrexBoard.movement_result(), TetrexBoard.board_preview()}
  def try_move_left(board_pid) do
    GenServer.call(board_pid, :try_move_left)
  end

  @spec try_move_right(pid()) :: {TetrexBoard.movement_result(), TetrexBoard.board_preview()}
  def try_move_right(board_pid) do
    GenServer.call(board_pid, :try_move_right)
  end

  @spec try_move_down(pid()) ::
          {TetrexBoard.movement_result(), TetrexBoard.board_preview(), non_neg_integer()}
  def try_move_down(board_pid) do
    GenServer.call(board_pid, :try_move_down)
  end

  @spec drop(pid()) :: {TetrexBoard.board_preview(), non_neg_integer()}
  def drop(board_pid) do
    GenServer.call(board_pid, :drop)
  end

  @spec rotate(pid()) :: TetrexBoard.board_preview()
  def rotate(board_pid) do
    GenServer.call(board_pid, :rotate)
  end

  @spec add_blocking_row(pid()) :: TetrexBoard.board_preview()
  def add_blocking_row(board_pid) do
    GenServer.call(board_pid, :add_blocking_row)
  end

  @spec remove_blocking_row(pid()) :: TetrexBoard.board_preview()
  def remove_blocking_row(board_pid) do
    GenServer.call(board_pid, :remove_blocking_row)
  end

  @spec hold(pid()) :: Board.board_preview()
  def hold(board_pid) do
    GenServer.call(board_pid, :hold)
  end

  # Server callbacks

  @impl true
  @spec init(init_args()) :: {:ok, %TetrexBoard{}}
  def init(opts) do
    {:ok,
     TetrexBoard.new(
       Keyword.get(opts, :height, 20),
       Keyword.get(opts, :width, 10),
       Keyword.get(opts, :random_seed, Enum.random(0..10_000_000))
     )}
  end

  @impl true
  def handle_call(:preview, _from, board) do
    preview = TetrexBoard.preview(board)
    {:reply, preview, board}
  end

  @impl true
  def handle_call({:new, height, width, seed}, _from, _board) do
    board = TetrexBoard.new(height, width, seed)
    preview = TetrexBoard.preview(board)

    {:reply, preview, board}
  end

  @impl true
  def handle_call(:try_move_left, _from, board) do
    {status, new_board} = TetrexBoard.try_move_active_left(board)
    preview = TetrexBoard.preview(new_board)
  end

  @impl true
  def handle_call(:try_move_right, _from, board) do
    {status, new_board} = Board.try_move_active_right(board)
    preview = TetrexBoard.preview(new_board)

    {:reply, {status, preview}, new_board}
  end

  @impl true
  def handle_call(:try_move_down, _from, board) do
    {status, new_board, num_lines_cleared} = TextrexBoard.try_move_active_down(board)
    preview = TetrexBoard.preview(new_board)

    {:reply, {status, preview, num_lines_cleared}, new_board}
  end

  @impl true
  def handle_call(:drop, _from, board) do
    {new_board, num_lines_cleared} = TetrexBoard.drop_active(board)
    preview = TetrexBoard.preview(new_board)

    {:reply, {preview, num_lines_cleared}, new_board}
  end

  @impl true
  def handle_call(:rotate, _from, board) do
    new_board = TetrexBoard.rotate_active(board)
    preview = TetrexBoard.preview(new_board)

    {:reply, preview, new_board}
  end

  @impl true
  def handle_call(:hold, _from, board) do
    new_board = TetrexBoard.hold_active(board)
    preview = TetrexBoard.preview(new_board)

    {:reply, preview, new_board}
  end

  @impl true
  def handle_call(:add_blocking_row, _from, board) do
    new_board = TetrexBoard.add_blocking_row(board)
    preview = TetrexBoard.preview(new_board)

    {:reply, preview, new_board}
  end

  def handle_call(:remove_blocking_row, _from, board) do
    new_board = TetrexBoard.remove_blocking_row(board)
    preview = TetrexBoard.preview(new_board)

    IO.inspect("REMOVE BLOCKING")
    {:reply, preview, new_board}
  end

  @impl true
  def handle_call(_, _from, board) do
    {:reply, :unknown_command, board}
  end

  @impl true
  def handle_cast(_, board) do
    {:noreply, board}
  end
end
