defmodule Cowboy do

  @moduledoc """
    Weber's weber server.
  """

  @doc """
  Start new cowboy http/web socket handler with given name and config
  from config.ex
  """
  @dependencies [:crypto, :public_key, :ssl, :mimetypes, :ranch, :cowlib, :cowboy]

  def start(config, handler) do

    Enum.map(@dependencies, &( :application.start(&1) ) )

    web_server_config = Keyword.fetch!(config, :webserver)

    [_host, port, acceptors, ssl] =
      [:http_host, :http_port, :acceptors, :ssl]
        |> Enum.map( &Keyword.fetch!(web_server_config, &1) )
    ws_mod = config |> Keyword.fetch!(:ws) |> Keyword.fetch!(:ws_mod)

    compress = Keyword.get(web_server_config, :use_gzip, false)

    dispatch = :cowboy_router.compile([{:_, [{'/_ws', Handler.WeberWebSocketHandler, ws_mod},
                                             {'/[...]', Handler.WeberReqHandler, {config, handler}}]}])

    if ssl do
        [cacertifile, certfile, keyfile] =
          [:cacertfile_path, :certfile_path, :keyfile_path]
            |> Enum.map( &Keyword.fetch!(web_server_config, &1) )
        {:ok, _} = :cowboy.start_https(:https, acceptors, [port: port, cacertfile: cacertifile,
                                                           certfile: certfile, keyfile: keyfile,
                                                           env: [dispatch: dispatch]])
    else
      :cowboy.start_http(:http, acceptors, [port: port], [env: [dispatch: dispatch],
                                                          compress: compress])
    end
  end
end