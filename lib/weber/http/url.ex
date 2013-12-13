defmodule Weber.Http.Url do

  @moduledoc """
    Parse REST url from Weber router, parse standard segments
    and url bindings.

    Url can be:

        * /user/0xAX
        * /user/0xAX/password/:password
        * /user?name=0xAX
        * /product?price=90&color=red

    ## Usage:

        getBinding("/user/0xAX/password/:password")

    Return:

        [segment: "user", segment: "0xAX", segment: "password", binding: "password"]
    """
    
  import List

  @doc """
    Parse url and returns url pathes and bindings.
  """
  def getBinding(url) do
    getBinding(url, [])
  end

  #
  #  match ':' in url
  #
  defp getBinding(<<58, rest :: binary>>, l) do
    case List.last(l) do
      {:segment, <<>>} -> l = delete(l, {:segment, <<>>})
      _ -> :pass
    end
        
    getBinding(rest, :lists.append([l, [binding: <<>>]]))    
  end

  #
  #  match '/' in url
  #
  defp getBinding(<<47, rest :: binary>>, l) do
    case List.last(l) do
      {:binding, <<>>} -> l = delete(l, {:binding, <<>>})
      _ -> :pass
    end

    getBinding(rest, :lists.append([l, [segment: <<>>]]))
  end

  #
  # match '?' in url
  #
  defp getBinding(<<63, rest :: binary>>, l) do
    getBinding(rest, :lists.append([l, [{:param, :undone, <<>>, <<>>}]]))
  end

  #
  # match '='
  #
  defp getBinding(<<61, rest :: binary>>, l) do
    {:param, :undone, k, v} = List.last(l)
    new_routes_list = List.delete(l, List.last(l))
    getBinding(rest, :lists.append(new_routes_list, keyreplace([List.last(l)], :undone, 1, {:param, :done, k, v})))
  end

  #
  # match '&'
  #
  defp getBinding(<<38, rest :: binary>>, l) do
    getBinding(rest, :lists.append([l, [{:param, :undone, <<>>, <<>>}]]))
  end

  #
  #  match [a-z A-Z 0-9] in url
  # 
  defp getBinding(<<s, rest :: binary>>, l) do
    case List.last(l) do
      {key, val} -> 
        getBinding(rest, :lists.reverse(keyreplace(:lists.reverse(l), key, 0, {key, val <> <<s>>})))
      {:param, :undone, key, val} ->
        getBinding(rest, :lists.reverse(keyreplace(:lists.reverse(l), :param, 0, {:param, :undone, key <> <<s>>, val})))
      {:param, :done, key, val} ->
        getBinding(rest, :lists.reverse(keyreplace(:lists.reverse(l), :param, 0, {:param, :done, key, val <> <<s>>})))
    end
  end

  #
  #  return parsed url with bindings
  #
  defp getBinding(<<>>, l) do
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
    Get binding's value list.
  """
  def getAllBinding(url, matched_url) when is_regex(matched_url) do
    []
  end
  
  def getAllBinding(url, matched_url) do
    zip = :lists.zip(getBinding(url), getBinding(matched_url))
    
    filterBindings = Enum.filter(zip, fn({{_, _}, {key, _}}) -> 
      case key == :binding do
        true -> true
        _    -> false
      end
    end)

    Enum.map(filterBindings, fn({{_key1, val1}, {_key2, val2}}) -> 
      {:erlang.binary_to_atom(val2, :utf8), val1}
    end)
  end

end