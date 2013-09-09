defmodule Mix.Tasks.Weber do
    use Mix.Task
    
    import Path
    import File
    import Mix.Generator

    @version Weber.Mixfile.project[:version]

    @shortdoc "Create a new weber project"

     @moduledoc """

        Create a new weber project template.

        Usage:

          mix weber /home/user/myWebApp - Creates myWebApp directory with weber project skeleton.
          mix weber --version - Prints weber version.
     """

    def run(["--version"]) do
        Mix.shell.info "Weber v#{@version}"
    end

    def run([args]) do
        # new project's root directory
        directoryName = args
        # create project directory
        case mkdir(directoryName) do
            :ok -> :ok
            err -> :io.format("[Weber error] Can't create project directory. Error: ~p~n", [err])
        end

        #
        # create project skeleton
        #
        path = absname directoryName
        basePath = basename directoryName
                
        create_file path <> <<"/README.md">>, (readme basePath)
        create_file path <> <<"/.gitignore">>, gitignore
        create_file path <> <<"/weber.conf">>, config
        create_file path <> <<"/mix.lock">>,   mixlock
        create_file path <> <<"/mix.exs">>, (project basePath)
        create_file path <> <<"/lib/app.ex">>, (app basePath)
        create_file path <> <<"/lib/route.ex">>, route

        create_directory path <> <<"/lib">>
        create_directory path <> <<"/lib/static">>
        create_directory path <> <<"/lib/views">>
        create_directory path <> <<"/lib/models">>
        create_directory path <> <<"/lib/controllers">>
        create_directory path <> <<"/lib/helpers">>
    
    end

    def route do
        """
        defmodule Route do
            
        end
        """
    end

    def readme(basePath) do
        """ 
        #{basePath}
        =====

        """
    end

    def gitignore do
        """
        /ebin
        /deps
        erl_crash.dump
        """
    end

    def config do
        """
        [
            {weber, [
                ]}
        ]
        """
    end

    def mixlock do
        from_file("../../../../mix.lock")    
    end

    def project(projectName) do
        proj = String.capitalize projectName

        """
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
                    applications: [:cowboy],
                    mod: {#{proj}, []}
                ]
            end

            defp deps do
                [ 
                    { :weber, github: "0xAX/weber" } 
                ]
            end
        end
        """
    end

    def app(projectName) do
        proj = String.capitalize projectName        
        """
        defmodule #{proj} do
             
        end
        """
    end 
end