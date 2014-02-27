defmodule Room do
  use GenServer.Behaviour

  import Weber.Session

  def start_link() do
    :gen_server.start_link({:local, :room}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:new_user, pid}, state) do
    {:noreply, [pid | state] }
  end

  def handle_cast({:delete_user, pid}, state) do
    {:noreply, :lists.delete(pid, state)}
  end

  def handle_cast({:send, pid, message, conn}, state) do
    case message do
      <<"get_login">> ->
        username = get_session(:username)
        send(pid, username)
      _ ->
        Enum.each(state, fn(user) ->
          send(user, message)
        end)
    end
    {:noreply, state}
  end

end
