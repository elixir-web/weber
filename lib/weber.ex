defmodule Weber do
  use Application.Behaviour

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
    Cowboy.start
    :ets.new(:req_storage, [:named_table, :public, :set, {:keypos, 1}])
    Weber.Session.SessionManager.start_link(Config.config)
    Weber.Localization.LocalizationManager.start_link(Config.config)
    Weber.Supervisor.start_link
  end

  @doc """
  Stop weber application.
  """
  def stop(_state) do
    :ok
  end

end