defmodule Weber.Session do
  
  @moduledoc """
  Weber session handler. Every process handle
  one use session.
  """

  use GenServer.Behaviour

  defrecord Session,
    session_life_time: nil,
    session_key: nil,
    session_id:  nil

  def start_link(session_life_time, session_key, session_id) do
    :gen_server.start_link(__MODULE__, [session_life_time, session_key, session_id], [])
  end

  def init([session_life_time, session_key, session_id]) do
  	:timer.send_after(session_life_time * 1000, :erlang.self, {:time_to_die, session_key, session_id})
    { :ok, Session.new session_life_time: session_life_time, session_key: session_key, session_id: session_id }
  end

  def handle_info({:time_to_die, _session_key, _session_id}, state) do
  	{:stop, :normal, :shutdown_ok, state}
  end

end