defmodule Mix.Tasks.Run do

    @moduledoc """
        Mix task for the running weber web application.
    
        Usage:

            * mix run
    """

    use Mix.Task

    def run(_) do
        app = :application.get_env(:application)
        :io.format("app ~p~n", [app])
    end

end
