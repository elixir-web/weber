defmodule Weber.Route do

  @moduledoc """
    This module handles routing of weber web application.
    Use 'route' macros for declaring routing.

    route when('/', Controller1, Action1)
              |> routes ('', Controller2, Action2)
              |> routes ('', Controller2, Action1)
              |> otherwise (404, Controller2, ActionNotFound)
  """

  @doc """
  """
  defmacro route(body) do          
        quote do:     
            unquote((fn(body) -> body end).(body))        
  end

  def routes() do
  end

  def otherwise do
  end
end