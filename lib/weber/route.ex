defmodule Weber.Route do

    @moduledoc """
      This module handles routing of weber web application.
      Use 'route' macros for declaring routing.

      @route on('/', Controller1, Action1)
             |> on ('', Controller2, Action2)
             |> on ('', Controller2, Action1)
             |> otherwise (404, Controller2, ActionNotFound)
    """

    import Weber.Http.Url

    @doc """
      Router attribute
    """
    def on(path, controller, action) do
        [[path: path, controller: controller, action: action]]
    end

    def on(routesList, path, controller, action) do
        :lists.append(routesList, [[path: path, controller: controller, action: action]])
    end

    @doc """
      Router attribute
    """
    def otherwise(path, controller, action) do
        on(path, controller, action)
    end

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

    @doc """
    
    """
    def match_routes_helper([], []) do
        true
    end

    def match_routes_helper([{_type, _path} | _parsed_path], []) do
        false
    end

    def match_routes_helper([], [{_route_type, _route_path} | _parsed_route_path]) do
        false
    end
    
    def match_routes_helper([{type, path} | parsed_path], [{route_type, route_path} | parsed_route_path]) do
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

end