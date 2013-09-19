defmodule Cowboy do
    
    @moduledoc """
        Starts cowboy instance.
    """

    def start(name, config) do
        :application.start(:crypto)
        :application.start(:public_key)
        :application.start(:ssl)
        :application.start(:ranch)
        :application.start(:cowlib)
        :application.start(:cowboy)

        {:webserver, web_server_config} = :lists.keyfind(:webserver, 1, config)
        {_, _host} = :lists.keyfind(:http_host, 1, web_server_config)
        {_, port} = :lists.keyfind(:http_port, 1, web_server_config)
        {_, acceptors} = :lists.keyfind(:acceptors, 1, web_server_config)

        {_, ws}      = :lists.keyfind(:ws, 1, web_server_config)
        {_, ws_port} = :lists.keyfind(:ws_port, 1, web_server_config)

        {_, ssl} = :lists.keyfind(:ssl, 1, web_server_config)
        
        dispatch = :cowboy_router.compile([{:_, [{:_, Handler.WeberReqHandler, name}]}])

        case ssl do
            true -> 
                {_, cacertifile} = :lists.keyfind(:cacertfile_path, 1, web_server_config)
                {_, certfile} = :lists.keyfind(:certfile_path, 1, web_server_config)
                {_, keyfile} = :lists.keyfind(:keyfile_path, 1, web_server_config)
            
                {:ok, _} = :cowboy.start_https(:https, acceptors, [{:port, port}, {:cacertfile, cacertifile},
                                                                   {:certfile,certfile}, {:keyfile, keyfile}], 
                                                                  [env: [dispatch: dispatch]])
            _ -> 
                {:ok, _} = :cowboy.start_http(:http, acceptors, [port: port], [env: [dispatch: dispatch]])
        end

        case ws do
            true ->
                wsDispatch = :cowboy_router.compile([{:_, [{:_, Handler.WeberWebSocketHandler, name}]}])
                {:ok, _} = :cowboy.start_http(:ws, acceptors, [port: ws_port], [env: [dispatch: wsDispatch]])
            _ -> :ok
        end
    end
end