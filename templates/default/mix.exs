defmodule #{proj}.Mixfile do
    use Mix.Project

    def project do
        [ 
            app: :#{projectName},
            version: "0.0.1",
            deps: deps
        ]
    end

    def application do
        [
            applications: [:weber],
            mod: {#{proj}, []}
        ]
    end

    defp deps do
        [ 
            { :weber, github: "0xAX/weber", compile: "mix deps.get && mix compile" } 
        ]
    end
end
