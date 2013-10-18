defmodule Weber.Supervisor do
  use Supervisor.Behaviour

  @moduledoc """
    Weber's root supervisor.
  """

  @doc """
  Starts Weber's root supervisor.
  """
  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  @doc """
  Starts weber application process.
  """
  def start_app(app_name, routes, root_directory, config) do
    :supervisor.start_child(__MODULE__, [app_name, routes, root_directory, config])
  end

  @doc """
  Weber's supervisor init/1 callback.
  """
  def init([]) do
    children = [ worker(Weber.App, [])]
    supervise children, strategy: :simple_one_for_one
  end

end