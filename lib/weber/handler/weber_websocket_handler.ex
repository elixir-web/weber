defmodule Handler.WeberWebSocketHandler do
  
  import Plug.Conn
  
  defrecord State, 
    ws_mod:   nil,
    conn:     nil

  @connection Plug.Adapters.Cowboy.Conn

  def init(_, _req, _opts) do
    {:upgrade, :protocol, :cowboy_websocket}
  end

  def websocket_init(_, req, ws_mod) do
    conn = @connection.conn(req, :tcp)
    conn = assign(conn, :req, req)
    Module.function(ws_mod, :websocket_init, 2).(self(), conn)
    {:ok, req, State.new ws_mod: ws_mod, conn: conn}
  end

  def websocket_handle({_, data}, req, state) do
    Module.function(state.ws_mod, :websocket_message, 3).(self(), data, state.conn)
    {:ok, req, state}
  end

  def websocket_info(msg, req, state) do
    {:reply, {:text, msg}, req, state}
  end

  def websocket_terminate(_reason, _req, state) do
    Module.function(state.ws_mod, :websocket_terminate, 2).(self(), state.conn)
    :ok
  end
end