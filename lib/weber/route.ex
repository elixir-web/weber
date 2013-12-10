defmodule Weber.Route do

  @moduledoc """
    This module handles routing of weber web application.
    Use 'route' macros for declaring routing.
    
      ## Example
    
        route on("ANY", '/', Controller1, Action1)
           |> on ("GET", '/user/:name', Controller2, Action2)
           |> on ("POST", "/user/add/:username", "Controller2#Action2")
           |> on ("GET", '/user/:name/delete', Controller2, Action1)
  """

  import String

  import Weber.Utils
  import Weber.Http.Url

  defmacro route(routeList) do
    quote do
      def unquote(:__route__)() do
        unquote(routeList)
      end
    end
  end

  def link(controller, action, bindings // []) do
    routes_with_same_controller = Enum.filter(Route.__route__, 
      fn(route) ->
        :lists.member({:controller, controller}, route) 
      end)
    routes_with_same_action = Enum.filter(routes_with_same_controller, 
      fn(route) ->
        :lists.member({:action, action}, route) 
      end) |> Enum.at(0)
    parsed_route = getBinding(Keyword.get(routes_with_same_action, :path))
    List.foldl(parsed_route, "", 
      fn({type, value}, acc) ->
        case type do
          :segment -> acc <> "/" <> value
          :binding -> acc <> "/" <> to_bin(Keyword.get(bindings, binary_to_atom(value)))
        end
      end)
  end

  @doc """
    Create list with router path.

    controllerAndAction - is controller and action in: :Controller#action format
  """
  def on(method, path, controllerAndAction) when is_binary(controllerAndAction) do
    [controller, action] = split(controllerAndAction, "#")
    [[method: method, path: path, controller: binary_to_atom(controller), action: binary_to_atom(action)]]
  end

  def on(routesList, method, path, controllerAndAction) when is_binary(controllerAndAction) do
    [controller, action] = split(controllerAndAction, "#")
    :lists.append(routesList, [[method: method, path: path, controller: binary_to_atom(controller), action: binary_to_atom(action)]])
  end

  @doc """
    Create list with router path.
  """
  def on(method, path, controller, action) do
    [[method: method, path: path, controller: controller, action: action]]
  end

  def on(routesList, method, path, controller, action) do
    :lists.append(routesList, [[method: method, path: path, controller: controller, action: action]])
  end
  
  @doc """
    Match current url path. Is it web application route or not
  """
  def match_routes(path, routes, req_method) do
    parsed_path = getBinding(path)

    Enum.filter(routes, 
      fn(route) -> 
        [method: method, path: p, controller: _controller, action: _action] = route
          case p do
            "404" ->
              false
            _ ->
              parsed_route_path = getBinding(p)
              case method do
                "ANY" -> match_routes_helper(parsed_path, parsed_route_path)
                _ -> (match_routes_helper(parsed_path, parsed_route_path) and (req_method == method))
              end
          end
      end)
  end
  
  defp match_routes_helper([], []) do
    true
  end

  defp match_routes_helper([{_type, _path} | _parsed_path], []) do
    false
  end

  defp match_routes_helper([], [{_route_type, _route_path} | _parsed_route_path]) do
    false
  end

  defp match_routes_helper([{:param, _, _key, _val} | _parsed_path], []) do
    true
  end
    
  defp match_routes_helper([{type, path} | parsed_path], [{route_type, route_path} | parsed_route_path]) do
    case type == route_type do
      true -> 
        case path == route_path do
          true -> match_routes_helper(parsed_path, parsed_route_path)
          false -> false
        end
      false -> 
        case route_type == :binding do
          true -> 
            case path do
              <<>> -> false
              _ -> match_routes_helper(parsed_path, parsed_route_path)
            end
          false -> false
        end
    end
  end

  defp match_routes_helper([{:param, _, _key, _val} | _parsed_path], [{_route_type, _route_path} | _parsed_route_path]) do
    true
  end

end
