defmodule Handler.WeberWebSocketHandler do
    
    def init(_, _req, opts) do
        {:upgrade, :protocol, :cowboy_websocket}
    end

    def websocket_init(_, req, {name, ws_mod}) do
        pid = self()
        {:ok, req, []}
    end

    def websocket_handle(data, req, state) do
        {:ok, req, state}
    end

    def websocket_terminate(reason, req, state) do
        pid = self()
        :ok
    end
end