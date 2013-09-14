defmodule Cowboy do
    
    @moduledoc """
        Starts cowboy instance.
    """

    def start(name, config) do
        :application.start(:crypto)
        :application.start(:ranch)
        :application.start(:cowlib)
        :application.start(:cowboy)

        {:webserver, web_server_config} = :lists.keyfind(:webserver, 1, config)
        {_, _host} = :lists.keyfind(:http_host, 1, web_server_config)
        {_, port} = :lists.keyfind(:http_host, 1, web_server_config)
        {_, acceptors} = :lists.keyfind(:acceptors, 1, web_server_config)
        
        dispatch = :cowboy_router.compile([{:_, [{:_, Handler.WeberReqHandler, name}]}])
        {:ok, _} = :cowboy.start_http(:http, acceptors, [port: port], [env: [dispatch: dispatch]])
    end
end