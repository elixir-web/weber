defmodule Weber do
  use Application.Behaviour

  @moduledoc """
  Main Weber module. Starts Weber application.
  """

  @doc """
  Start new Weber application instance with given
  application name, route list, web application's root
  directory and config from config.ex.
  """
  def run_weber(app_name, routes, root_directory, config) do
    start([], [])
    Weber.Session.SessionManager.start_link(config)
    Weber.Localization.LocalizationManager.start_link(config)
    Weber.Supervisor.start_app(app_name, routes, root_directory, config)
  end

  @doc """
  Start weber application
  """
  def start(_type, _args) do
    :application.start(:mimetypes)
    Weber.Supervisor.start_link
  end

  @doc """
  Stop weber application.
  """
  def stop(_state) do
    :ok
  end

end