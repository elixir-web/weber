defmodule Weber.Http.Params do
  @moduledoc """
  API for getting request params

    ## Example

    defmodule Simplechat.Main.Login do

      import Weber.Http.Params

      def render_login("GET", conn) do
        
        # get body request
        body = get_body(conn)
        
        #
        # Do something with param
        #
        {:render, [project: "SimpleChat"], []}
      end
      
    end
  """
  
  import Plug.Conn.Query

  @doc """
    Return the host binary string.
  """
  def get_host(conn) do
    conn.host
  end

  @doc """
    Return HTTP method
  """
  def get_method(conn) do
    conn.method
  end 

  @doc """
    Return the path binary string.
  """
  def get_path(conn) do
    conn.path_info
  end

  @doc """
    Return the header value for the given key.
  """
  def get_header(name, conn) do
    headers = conn.req_headers
    
    header_value(name, headers)
  end

  @doc """
    Get all headers
  """
  def get_headers(conn) do
    conn.req_headers
  end

  @doc """
    Get cookie
  """
  def get_cookie(name, conn) do
    :cowboy_req.cookie(name, conn.assigns[:req]) |> elem(0)
  end

  def get_cookie_p(name, req) do
    :cowboy_req.cookie(name, req) |> elem(0)  
  end

  @doc """
    Return the full list of cookie values.
  """
  def cookies(conn) do
    :cowboy_req.cookies(conn.assigns[:req]) |> elem(0)
  end

  @doc """
    Get body
  """
  def get_body(conn) do
    req = conn.assigns[:req]
    case :cowboy_req.has_body(req) do
      false -> []
      true -> :cowboy_req.body(req) |> elem(1)
    end
  end

  @doc """
    Get parameter value by key from query string.
  """
  def param(key, conn) do
    req = conn.assigns[:req]
    {val, _} = :cowboy_req.qs_val(key, req)
    val
  end

  #
  # Search headers for specified key, if found return the value otherwise return nil
  #
  defp header_value(_header, []), do: nil
  defp header_value(header, [first | rest]) do
    case first do
      {^header, value} -> value
      _ -> header_value(header, rest)
    end
  end
end
