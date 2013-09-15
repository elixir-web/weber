defmodule Simpletodo.Mixfile do
    use Mix.Project

    def project do
        [ 
            app: :simpleTodo,
            version: "0.0.1",
            deps: deps
        ]
    end

    def application do
        [
            applications: [:weber],
            mod: {Simpletodo, []}
        ]
    end

    defp deps do
        [ 
            { :weber, github: "0xAX/weber", compile: "mix deps.get && mix compile" } 
        ]
    end
end
