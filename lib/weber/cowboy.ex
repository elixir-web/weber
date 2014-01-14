defmodule Cowboy do
    
  @moduledoc """
    Weber's weber server.
  """

  @doc """
  Start new cowboy http/web socket handler with given name and config
  from config.ex
  """
  def start(config) do
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
    {_, ssl}       = :lists.keyfind(:ssl, 1, web_server_config)
    case :lists.keymember(:use_gzip, 1, web_server_config) do
      true -> {_, compress} = :lists.keyfind(:use_gzip, 1, web_server_config)
      _    -> compress = false
    end
    
    dispatch = case :lists.keyfind(:ws, 1, config) do
      {:ws, ws_config} ->
        {_, ws_mod}  = :lists.keyfind(:ws_mod, 1, ws_config)
        :cowboy_router.compile([{:_, [{'/_ws', Handler.WeberWebSocketHandler, ws_mod}, 
                                      {'/[...]', Handler.WeberReqHandler, config}]}])
      _ ->
        :cowboy_router.compile([{:_, [{'/[...]', Handler.WeberReqHandler, config}]}])
    end
     
    case ssl do
      true -> 
        {_, cacertifile} = :lists.keyfind(:cacertfile_path, 1, web_server_config)
        {_, certfile} = :lists.keyfind(:certfile_path, 1, web_server_config)
        {_, keyfile} = :lists.keyfind(:keyfile_path, 1, web_server_config)
        {:ok, _} = :cowboy.start_https(:https, acceptors, [{:port, port}, {:cacertfile, cacertifile},
                                                           {:certfile,certfile}, {:keyfile, keyfile}], 
                                                           [env: [dispatch: dispatch]])
      _ -> 
        {:ok, _} = :cowboy.start_http(:http, acceptors, [port: port], [env: [dispatch: dispatch],
                                                                       compress: compress])
    end
  end
end
