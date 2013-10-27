defmodule Simplechat.Main.Chat do

  import Weber.Session

  def render_chat([]) do
    case get_session(:username) do
      [] -> {:redirect, "/"}
      username -> {:render, [username: username], []}
    end
  end
  
  def websocket_init(pid) do
    :gen_server.cast(:room, {:new_user, pid})
  end

  def websocket_message(pid, message) do
    :gen_server.cast(:room, {:send, pid, message})
  end

  def websocket_terminate(pid) do
    :gen_server.cast(:room, {:delete_user, pid})
  end

end