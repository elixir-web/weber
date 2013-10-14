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
    :ets.new(:cookie_storage, [:named_table, :public, :bag])
    { :ok, SessionManager.new config: config }
  end

  def handle_call({:create_new_session, session_id, pid}, _from, state) do
    {_, session_config} = :lists.keyfind(:session, 1, state.config)
    {_, max_age} = :lists.keyfind(:max_age, 1, session_config)
    Weber.Session.start_link(max_age, session_id)

    case :ets.match_object(:cookie_storage, {:_, pid, :_}) do
      [] ->
        locale = case :lists.keyfind(:localization, 1, state.config) do
          false -> 
            []
          {:localization, localization_config} ->
            {_, default_locale}  = :lists.keyfind(:default_locale, 1, localization_config)
            default_locale
        end
        :ets.insert(:cookie_storage, {session_id, pid, [locale: locale]})
        {:reply, session_id, state}
      [{old_session_id, _pid, _opts}] -> 
        {:reply, old_session_id, state}
    end
  end

end