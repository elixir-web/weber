defmodule Simplechat.Main.Login do
  
  import Weber.Session

  use Weber.Controller

  layout false

  def render_login([]) do
    {:render, [], []}
  end

  def join([username: username]) do
    set_session_val(:username, username)
    {:json, [result: :ok, username: username], []}
  end

end