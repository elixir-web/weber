defmodule Weber.Session.SessionManager do
  
  @moduledoc """
  Session manager process. Create new session's process,
  manages all sessions.
  """

  use GenServer.Behaviour

  defrecord SessionManager,
    config:  nil
    
  def start_link(config) do
    :gen_server.start_link({:local, :session_manager}, __MODULE__, [config], [])
  end

  def init([config]) do
    { :ok, SessionManager.new config: config }
  end

  def handle_cast({:create_new_session, session_id}, state) do
    {_, session_config} = :lists.keyfind(:session, 1, state.config)
    {_, session_life_time}     = :lists.keyfind(:session_life_time, 1, session_config)
    {_, session_key}     = :lists.keyfind(:session_key, 1, session_config)
    Weber.Session.start_link(session_life_time, session_key, session_id)
  end

end