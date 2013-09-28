defmodule Weber.Http.Params do
  @moduledoc """
  API for getting request params
  """
  
  @doc """
    Return HTTP version
  """
  def get_version do
    case :ets.lookup(:req_storage, self) do
      [] -> []
      [{_, req}] ->
        {v, _} = :cowboy_req.version(req)
        v
    end
  end

  @doc """
    Return the peer address and port number of the remote host.
  """
  def get_peer do
    case :ets.lookup(:req_storage, self) do
      [] -> []
      [{_, req}] ->
        {{address, port}, _} = :cowboy_req.peer(req)
        {address, port}
    end
  end

  @doc """
    Return the host binary string.
  """
  def get_host do
    case :ets.lookup(:req_storage, self) do
      [] -> []
      [{_, req}] ->
        {host, _} = :cowboy_req.host(req)
        host
    end
  end

  @doc """
    Return the port.
  """
  def get_port do
    case :ets.lookup(:req_storage, self) do
      [] -> []
      [{_, req}] ->
        {port, _} = :cowboy_req.port(req)
        port
    end
  end

  @doc """
    Return the path binary string.
  """
  def get_path do
    case :ets.lookup(:req_storage, self) do
      [] -> []
      [{_, req}] ->
        {path, _} = :cowboy_req.path(req)
        path
    end
  end

  @doc """
    Return the header value for the given key.
  """
  def get_header(name) do
    case :ets.lookup(:req_storage, self) do
      [] -> []
      [{_, req}] ->
        {header, _} = :cowboy_req.header(name, req)
        header
    end
  end

  @doc """
    Get all headers
  """
  def get_headers do
    case :ets.lookup(:req_storage, self) do
      [] -> []
      [{_, req}] ->
        {headers, _} = :cowboy_req.headers(req)
        headers
    end
  end

  @doc """
    Get cookie
  """
  def get_cookie(name) do
    case :ets.lookup(:req_storage, self) do
      [] -> []
      [{_, req}] ->
        {cookie, _} = :cowboy_req.cookie(name, req)
        cookie
    end
  end

  @doc """
    Return the full list of cookie values.
  """
  def cookies do
    case :ets.lookup(:req_storage, self) do
      [] -> []
      [{_, req}] ->
        {cookie, _} = :cowboy_req.cookies(req)
        cookie
    end
  end

  @doc """
    Get body
  """
  def body do
    case :ets.lookup(:req_storage, self) do
      [] -> []
      [{_, req}] ->
        case :cowboy_req.body(req) do
            {:ok, body, _} -> body
            {:error, error} -> error
        end
    end
  end

end