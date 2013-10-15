defmodule Weber.Session do
  
  @moduledoc """
  Weber session handler. Every process handle
  one use session.
  """

  use GenServer.Behaviour

  import Weber.Http.Params

  defrecord Session,
    session_life_time: nil,
    session_id:  nil

  def start_link(session_life_time, session_id) do
    :gen_server.start_link(__MODULE__, [session_life_time, session_id], [])
  end

  def init([session_life_time, session_id]) do
    :timer.send_after(session_life_time * 1000, :erlang.self, {:time_to_die, session_id})
    { :ok, Session.new session_life_time: session_life_time, session_id: session_id }
  end

  def handle_info({:time_to_die, session_id}, state) do
    :ets.delete(:cookie_storage, session_id)
    {:stop, :normal, state}
  end

  def get_session do
    case get_session_helper() do
      [] -> []
      [{_, _, opts}] -> opts
    end
  end

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

  def set_session_val(key, val) do
    case get_session_helper() do
      [] -> []
      [{s, pid, opts}] ->
        :ets.delete(:cookie_storage, s)
        :ets.insert(:cookie_storage, {s, pid, [{key, val} | opts]})
        :ok
    end
  end

  defp get_session_helper do
    cookie = get_cookie("weber")

    case cookie do
      :undefined -> []
      _ ->
        sessions_list = :ets.tab2list(:cookie_storage)
        Enum.filter(sessions_list, 
          fn({s, _, _}) ->
            :erlang.list_to_binary(:lists.concat(:lists.concat(:erlang.binary_to_list(s)))) == cookie
          end)
    end

  end

end