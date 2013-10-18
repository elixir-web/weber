defmodule Weber.Route do

  @moduledoc """
    This module handles routing of weber web application.
    Use 'route' macros for declaring routing.
    
      ## Example
    
        @route on('/', Controller1, Action1)
          |> on ('/user/:name', Controller2, Action2)
          |> on ("/user/add/:username", "Controller2#Action2")
          |> on ('/user/:name/delete', Controller2, Action1)
          |> otherwise (404, Controller2, ActionNotFound)
  """

  import String
  import Weber.Http.Url

  @doc """
    Create list with router path.

    controllerAndAction - is controller and action in: :Controller#action format
  """
  def on(path, controllerAndAction) when is_binary(controllerAndAction) do
    [controller, action] = split(controllerAndAction, "#")
    [[path: path, controller: binary_to_atom(controller, :utf8), action: binary_to_atom(action, :utf8)]]
  end

  @doc """
    Add new route path to the routes list.

    controllerAndAction - is controller and action in: :Controller#action format
  """
  def on(routesList, path, controllerAndAction) when is_binary(controllerAndAction) do
    [controller, action] = split(controllerAndAction, "#")
    :lists.append(routesList, [[path: path, controller: binary_to_atom(controller, :utf8), action: binary_to_atom(action, :utf8)]])
  end

  @doc """
    Create list with router path.
  """
  def on(path, controller, action) do
    [[path: path, controller: controller, action: action]]
  end

  @doc """
    Add new route path to the routes list.
  """
  def on(routesList, path, controller, action) do
    :lists.append(routesList, [[path: path, controller: controller, action: action]])
  end

  @doc """
    Create list with router path.

    controllerAndAction - is controller and action in: :Controller#action format
  """
  def otherwise(path, controllerAndAction) when is_binary(controllerAndAction) do
    on(path, controllerAndAction)
  end

  @doc """
    Add new route path to the routes list.

    controllerAndAction - is controller and action in: :Controller#action format    
  """
  def otherwise(routesList, path, controllerAndAction) when is_binary(controllerAndAction) do
    on(routesList, path, controllerAndAction)
  end

  @doc """
    Create list with router path.
  """
  def otherwise(path, controller, action) do
    on(path, controller, action)
  end

  @doc """
    Add new route path to the routes list.
  """
  def otherwise(routesList, path, controller, action) do
    on(routesList, path, controller, action)
  end

  @doc """
    Match current url path. Is it web application route or not
  """
  def match_routes(path, routes) do
    parsed_path = getBinding(path)
    Enum.filter(routes, 
      fn(route) -> 
        [path: p, controller: _controller, action: _action] = route
          case p do
            "404" ->
              false
            _ ->
              parsed_route_path = getBinding(p)
              match_routes_helper(parsed_path, parsed_route_path)
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
          true -> match_routes_helper(parsed_path, parsed_route_path)
          false -> false
        end
    end
  end

  defp match_routes_helper([{:param, _, _key, _val} | _parsed_path], [{_route_type, _route_path} | _parsed_route_path]) do
    true
  end

end