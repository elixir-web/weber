defmodule Handler.WeberReqHandler do
    
    @moduledoc """
        Weber http request cowboy handler.
    """

    import Weber.Route

    def init(_transport, req, []) do

        {:ok, req, nil}
    end

    def handle(req, state) do
        # get method
        {method, req2} = :cowboy_req.method(req)
        # get path
        {path, req3} = :cowboy_req.path(req2)
        # get routes
        routes = :gen_server.call(:app, :routes)

        match_routes(path, routes)

        {:ok, req4} = :cowboy_req.reply(200, [], "Hello world!", req3)
        {:ok, req4, state}
    end

    def terminate(_reason, _req, _state) do
        :ok
    end
end