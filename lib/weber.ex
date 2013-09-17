defmodule Weber do
    use Application.Behaviour

    @moduledoc """
        Main weber application module.
    """

    def run_weber(app_name, routes, root_directory, config) do
        start([], [])
        Weber.Supervisor.start_app(app_name, routes, root_directory, config)
    end

    def start(_type, _args) do
        :application.start(:mimetypes)
        Weber.Supervisor.start_link
    end

    def stop(_state) do
        :ok
    end

end