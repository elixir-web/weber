defmodule Cowboy do
    
  @moduledoc """
    Weber's weber server.
  """

  @doc """
  Start new cowboy http/web socket handler with given name and config
  from config.ex
  """
  def start do
    config = Config.config

    :application.start(:crypto)
    :application.start(:public_key)
    :application.start(:ssl)
    :application.start(:mimetypes)
    :application.start(:ranch)
    :application.start(:cowlib)
    :application.start(:cowboy)
        
    {:webserver, web_server_config} = :lists.keyfind(:webserver, 1, config)
    {_, _host}     = :lists.keyfind(:http_host, 1, web_server_config)
    {_, port}      = :lists.keyfind(:http_port, 1, web_server_config)
    {_, acceptors} = :lists.keyfind(:acceptors, 1, web_server_config)
    {_, ssl}     = :lists.keyfind(:ssl, 1, web_server_config)

    {:ws, ws_config} = :lists.keyfind(:ws, 1, config)
    {_, ws_mod}  = :lists.keyfind(:ws_mod, 1, ws_config)
          
    dispatch = :cowboy_router.compile([{:_, [{'/_ws', Handler.WeberWebSocketHandler, ws_mod}, 
                                             {'/[...]', Handler.WeberReqHandler, []}]}])

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
  end
end