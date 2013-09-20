defmodule Mix.Tasks.Weber do

    @moduledoc """

       Create a new weber project template.

       Usage:

         mix weber /home/user/myWebApp - Creates myWebApp directory with weber project skeleton.
         mix weber --version - Prints weber version.
    """

    @shortdoc "Create a new weber project"

    use Mix.Task
    
    import Path
    import File
    import Mix.Generator

    @version Weber.Mixfile.project[:version]
    
    def run([]) do
        usage
    end

    def run(["--help"]) do
        usage
    end

    def run(["--version"]) do
        Mix.shell.info "Weber v#{@version}"
    end

    def usage do
        """
        Usage:

          mix weber.new /home/user/testWebApp -- creates new weber web application
          mix weber --version -- shows weber version
          mix weber --help    -- show weber help
          mix weber --run     -- runs current weber web application
        """
    end

    defmodule New do
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

            create_directory path <> <<"/lib">>
            create_directory path <> <<"/lib/static">>
            create_directory path <> <<"/lib/views">>
            create_directory path <> <<"/lib/models">>
            create_directory path <> <<"/lib/controllers">>
            create_directory path <> <<"/lib/helpers">>
            create_directory path <> <<"/test">>

            create_file path <> <<"/start.sh">>, (start directoryName)
            create_file path <> <<"/README.md">>, (readme basePath)
            create_file path <> <<"/.gitignore">>, gitignore
            create_file path <> <<"/mix.exs">>, (project basePath)
            create_file path <> <<"/lib/app.ex">>, (app basePath)
            create_file path <> <<"/lib/route.ex">>, (route basePath)
            create_file path <> <<"/lib/config.ex">>, config
            create_file path <> <<"/lib/controllers/main.ex">>, main_controller(basePath)
            create_file path <> <<"/lib/views/main.html">>, main_template(basePath)
            create_file path <> <<"/test/test_helper.exs">>, test
            create_file path <> <<"/test/">> <> (String.capitalize basePath) <> <<"test.exs">>, test_app(basePath)

            chmod(path <> "/start.sh", 0755)

        end

        def route(app) do
            proj = String.capitalize app
            """
            defmodule Route do

                import Weber.Route

                @route on("/", :#{proj}.Main, :action)

                def get_route do
                    @route
                end
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
            """
        end

        def app(projectName) do
            proj = String.capitalize projectName        
            """
            defmodule #{proj} do

                import Weber

                def start(_type, _args) do
                    {:ok, root} = :file.get_cwd()
                    run_weber(:#{proj}, Route.get_route, root, Config.config)
                end

                def stop(_state) do
                   :ok
                end
            end
            """
        end 

        def config do
            """
            defmodule Config do 
        
                def config do
                    [webserver: 
                        [http_host: "localhost", 
                         http_port: 8080,
                         acceptors: 100,
                         ssl: false,
                         cacertfile_path: "",
                         certfile_path: "",
                         keyfile_path: "",

                         #
                         # websocket settings
                         #
                         ws: true,
                         ws_port: 8800,
                         ws_mod: :Handler 
                        ]
                    ]
                end

            end
            """
        end

        def main_controller(app) do
            proj = String.capitalize(app) <> ".Main" 
            """
            defmodule #{proj} do

                def action("GET", []) do
                    {:render, [project: "#{app}"]}
                end
                    
            end
            """
        end

        def main_template(app) do
            proj = String.capitalize(app) 
            """
            <!DOCTYPE HTML>
            <html>
                <head>
                    <title>#{proj}</title>
                </head>
             
                <body>
                    <span>Hello, <%= project %></span> 
                </body>
            </html>
            """
        end

        def start(directoryName) do
            """
            #!/usr/bin/env sh

            if [ ! -f deps ]; then
                mix deps.get && mix compile
            fi

            export ERL_LIBS="$ERL_LIBS:#{directoryName}"
            exec iex -S mix
            """
        end

        def test() do
            """
            ExUnit.start
            """
        end

        def test_app(app) do
            path = (String.capitalize app) <> "Test"
            """
            defmodule #{path} do
                use ExUnit.Case

                test "the truth" do
                    assert(true)
                end
            end
            """
        end
    end

end