defmodule Handler.WeberWebSocketHandler do
    
  defrecord State, 
    ws_mod:   nil

  def init(_, _req, _opts) do
    {:upgrade, :protocol, :cowboy_websocket}
  end

  def websocket_init(_, req, ws_mod) do
    Module.function(ws_mod, :websocket_init, 1).(self())
    {:ok, req, State.new ws_mod: ws_mod}
  end

  def websocket_handle({_, data}, req, state) do
    Module.function(state.ws_mod, :websocket_message, 2).(self(), data)
    {:ok, req, state}
  end

  def websocket_info(msg, req, state) do
    {:reply, {:text, msg}, req, state}
  end

  def websocket_terminate(_reason, _req, state) do
    Module.function(state.ws_mod, :websocket_terminate, 1).(self())
    :ok
  end
end