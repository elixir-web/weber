defmodule Handler.WeberWebSocketHandler do
    
    def init(_, _req, _opts) do
        {:upgrade, :protocol, :cowboy_websocket}
    end

    def websocket_init(_, req, _opts) do
        {:ok, req, []}
    end

    def websocket_handle(data, req, state) do
        {:ok, req, state}
    end

    def websocket_terminate(reason, req, state) do
        :ok
    end
end