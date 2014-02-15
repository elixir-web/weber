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
  def start(type, _args) do
    # start lager
    case :lists.keyfind(:log, 1, Config.config) do
      {:log, true} ->

        [:compiler, :syntax_tools, :goldrush, :lager, :exlager]
          |> Enum.map(&(:ok = :application.start(&1) ) )
      _ ->
        :ok
    end
    
    # check handler
    handler = case Keyword.get(Config.config, :reload) do
      true ->
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

    case type do
      :test ->
        :pass
      _ ->
        # start reloader
        Weber.Reload.start
        # enable reloader
        Weber.Reload.enable
    end
    # return
    {:ok, self}
  end

  @doc """
  Stop weber application.
  """
  def stop(_state) do
    :ok
  end

end