defmodule Weber.Route do

    @moduledoc """
      This module handles routing of weber web application.
      Use 'route' macros for declaring routing.

      route when('/', Controller1, Action1)
                |> on ('', Controller2, Action2)
                |> on ('', Controller2, Action1)
                |> otherwise (404, Controller2, ActionNotFound)
    """

    @doc """
      route macros - for routing defenition in user
      weber web application.
    """
    defmacro route(body) do          
        quote do:     
            unquote((fn(body) -> body end).(body))        
    end

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
end