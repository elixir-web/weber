defmodule Handler.WeberReqHandler do
    
    @moduledoc """
        Weber http request cowboy handler.
    """

    import EEx
    import Weber.Route

    defrecord State, 
        app_name: nil

    def init(_transport, req, name) do
        {:ok, req, State.new app_name: name}
    end

    def handle(req, state) do
        # get method
        {method, req2} = :cowboy_req.method(req)
        # get path
        {path, req3} = :cowboy_req.path(req2)

        # get routes
        routes = :gen_server.call(state.app_name, :routes)
        :io.format("~p~n", [match_routes(path, routes)])
        {:ok, req4} = :cowboy_req.reply(200, [], "Hello world!", req3)
        {:ok, req4, state}
    end



    def terminate(_reason, _req, _state) do
        :ok
    end
end