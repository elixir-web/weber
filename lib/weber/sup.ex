defmodule Weber.Supervisor do
    use Supervisor.Behaviour

    @moduledoc """
        Weber's root supervisor.
    """

    def start_link do
        :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
    end

    def start_app(routes, root_directory, config) do
        :supervisor.start_child(__MODULE__, [routes, root_directory, config])
    end

    def init([]) do
      children = [ worker(Weber.App, [])]
      supervise children, strategy: :simple_one_for_one
    end

end