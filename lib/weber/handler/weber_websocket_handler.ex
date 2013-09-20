defmodule Handler.WeberWebSocketHandler do
    
    defrecord State, 
        app_name: nil,
        ws_mod:   nil

    def init(_, _req, opts) do
        {:upgrade, :protocol, :cowboy_websocket}
    end

    def websocket_init(_, req, {name, ws_mod}) do
        Module.function(ws_mod, :websocket_init, 1).(self())
        {:ok, req, State.new app_name: name, ws_mod: ws_mod}
    end

    def websocket_handle({_, data}, req, state) do
        Module.function(state.ws_mod, :websocket_message, 2).(self(), data)
        {:ok, req, state}
    end

    def websocket_info(msg, req, state) do
        {:reply, {:text, msg}, req, state}
    end

    def websocket_terminate(reason, req, state) do
        Module.function(state.ws_mod, :websocket_terminate, 1).(self())
        :ok
    end
end