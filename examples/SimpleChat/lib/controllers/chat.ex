defmodule Simplechat.Main.Chat do

  import Weber.Session
  
  def render_chat([], conn) do
    case get_session(conn, :username) do
      [] -> {:redirect, "/"}
      username -> {:render, [username: username], []}
    end
  end
  
  def websocket_init(pid, conn) do
    :gen_server.cast(:room, {:new_user, pid})
  end

  def websocket_message(pid, message, conn) do
    :gen_server.cast(:room, {:send, pid, message, conn})
  end

  def websocket_terminate(pid, conn) do
    :gen_server.cast(:room, {:delete_user, pid})
  end

end