defmodule Weber do

  use Application.Behaviour

  require Weber.Templates.ViewsLoader

  @moduledoc """
  Main Weber module. Starts Weber application.
  """

  @doc """
  Start new Weber application instance with given
  application name, web application's root
  directory and config from config.ex.
  """
  def run_weber do
    start([], [])
  end

  @doc """
  Start weber application
  """
  def start(_type, _args) do
    # start lager
    case :lists.keyfind(:log, 1, Config.config) do
      {:log, true} ->
        :ok = :application.start(:compiler)
        :ok = :application.start(:syntax_tools)
        :ok = :application.start(:goldrush)
        :ok = :application.start(:lager)
        :ok = :application.start(:exlager) 
      _ ->
        :ok
    end
    # check handler
    handler = case :lists.keyfind(:reload, 1, Config.config) do
      {:reload, true} ->
        # start reloader
        Weber.Reload.start
        # enable reloader
        Weber.Reload.enable

        :Handler.WeberReqHandler.Development.Result
      _ ->
        :Handler.WeberReqHandler.Result
    end
    
    # start cowboy
    Cowboy.start(Config.config, handler)
    # start session manager
    Weber.Session.SessionManager.start_link(Config.config)
    # start localization manager
    Weber.Localization.LocalizationManager.start_link(Config.config)
  end

  @doc """
  Stop weber application.
  """
  def stop(_state) do
    :ok
  end

end