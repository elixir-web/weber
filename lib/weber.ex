defmodule Weber do
    use Application.Behaviour

    @moduledoc """
        Main weber application module.
    """

    def run_weber(routes, root_directory, config) do
        start([], [])
        Weber.Supervisor.start_app(routes, root_directory, config)
    end

    def start(_type, _args) do
        Weber.Supervisor.start_link
    end

    def stop(_state) do
        :ok
    end

end