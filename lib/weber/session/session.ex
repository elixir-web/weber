defmodule Weber.Session do
  
  @moduledoc """
  Weber session handler. Every process handle one use session.
  """

  use GenServer.Behaviour

  import Weber.Http.Params

  defrecord Session,
    session_life_time: nil,
    session_id:  nil

  @doc """
  Start new session.
  """
  def start_link(session_life_time, session_id) do
    :gen_server.start_link(__MODULE__, [session_life_time, session_id], [])
  end

  @doc """
  gen_server init/1 callback.
  """
  def init([session_life_time, session_id]) do
    :timer.send_after(session_life_time * 1000, :erlang.self, {:time_to_die, session_id})
    { :ok, Session.new session_life_time: session_life_time, session_id: session_id }
  end

  @doc """
  Session timeout expired.
  """
  def handle_info({:time_to_die, session_id}, state) do
    :ets.delete(:cookie_storage, session_id)
    {:stop, :normal, state}
  end

  @doc """
  Get current session parameters.
  """
  def get_session do
    case get_session_helper() do
      [] -> []
      [{_, _, opts}] -> opts
    end
  end

  @doc """
  Get session parameter value by key.
  """
  def get_session(key) do
    case get_session_helper() do
      [] -> []
      [{_, _, opts}] -> 
        val = :lists.keyfind(key, 1, opts)
        case val do
          false -> []
          {_, v} -> v
        end
    end
  end

  @doc """
  Set session parameter.
  """
  def set_session_val(key, val) do
    case get_session_helper() do
      [] -> []
      [{s, pid, opts}] ->
        :ets.delete(:cookie_storage, s)
        :ets.insert(:cookie_storage, {s, pid, [{key, val} | :lists.keydelete(key, 1, :lists.keydelete(key, 1, opts))]})
        :ok
    end
  end

  defp get_session_helper do
    cookie = get_cookie("weber")
    
    case cookie do
      :undefined -> []
      _ ->
        Enum.filter(:ets.tab2list(:cookie_storage), 
          fn({session_id, _, _}) ->
            session_id == cookie
        end)
    end
  end
end