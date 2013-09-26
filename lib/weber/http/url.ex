defmodule Weber.Http.Url do

  @moduledoc """
    Parse REST url from Weber router, parse standard segments
    and url bindings.

    Url can be:

       * /user/0xAX
       * /user/0xAX/password/:password

    Usage:

      getBinding("/user/0xAX/password/:password")

    Return:

      [segment: "user", segment: "0xAX", segment: "password", binding: "password"]
    """
    
  import List

  def getBinding(url) do
    getBinding(url, [])
  end

  @doc """
    match ':' in url
  """
  def getBinding(<<58, rest :: binary>>, l) do
    case List.last(l) do
      {:segment, <<>>} -> l = delete(l, {:segment, <<>>})
      _ -> :pass
    end
        
    getBinding(rest, :lists.append([l, [binding: <<>>]]))    
  end

  @doc """
    match '/' in url
  """
  def getBinding(<<47, rest :: binary>>, l) do
    case List.last(l) do
      {:binding, <<>>} -> l = delete(l, {:binding, <<>>})
      _ -> :pass
    end
    getBinding(rest, :lists.append([l, [segment: <<>>]]))
  end

  @doc """
    match [a-z A-Z 0-9] in url
  """
  def getBinding(<<s, rest :: binary>>, l) do
    {key, val} = List.last(l)
    getBinding(rest, :lists.reverse(keyreplace(:lists.reverse(l), key, 0, {key, val <> <<s>>})))
  end

  @doc """
    return parsed url with bindings
  """
  def getBinding(<<>>, l) do
    case l do
      [segment: <<>>] -> l
      _ ->
        case List.last(l) do
          {:segment, <<>>} -> delete(l, {:segment, <<>>})
          _ -> l
        end
    end
  end

  @doc """
    Get all url's bindings
  """
  def getAllBinding(url, matched_url) do
    parsed_url = getBinding(url)
    parsed_matched_url = getBinding(matched_url)
        
    zip = :lists.zip(parsed_url, parsed_matched_url)

    filterBindings = Enum.filter(zip, fn({{_, _}, {key, _}}) -> 
      case key == :binding do
        true -> true
        _    -> false
      end
    end)

    Enum.map(filterBindings, fn({{_key1, val1}, {_key2, val2}}) -> 
      {val2, val1}
    end)
  end

end