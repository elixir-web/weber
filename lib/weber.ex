defmodule Weber do
    use Application.Behaviour

    @moduledoc """
        Main weber application.
    """

    def start(_type, _args) do
        Weber.Supervisor.start_link
    end

    def stop(_state) do
        :ok
    end

end