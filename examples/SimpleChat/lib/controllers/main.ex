defmodule Simplechat.Main.Login do
  
  import Weber.Session

  use Weber.Controller

  layout false

  def render_login([], _conn) do
    {:render, [], []}
  end

  def join([username: username], conn) do
    set_session_val(conn, :username, username)
    {:json, [result: :ok, username: username], []}
  end

end