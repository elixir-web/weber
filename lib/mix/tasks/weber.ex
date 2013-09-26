defmodule Mix.Tasks.Weber do

    @moduledoc """

       Create a new weber project template.

       Usage:

         mix weber.new /home/user/myWebApp - Creates myWebApp directory with weber project skeleton.
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
            baseName = basename directoryName

            vars = HashDict.new [
                {"path", path},
                {"proj", String.capitalize(baseName)},
                {"projectName", baseName}
            ]

            template = "default"
            {:ok, pwd} = cwd
            skelRoot = pwd <> "/templates/" <> template
            cd skelRoot
            skelFiles = Weber.Utils.get_all_files(".")
            lc file inlist skelFiles do
                baseFile = String.slice(file, 1, 1024)
                # TODO: fix to allow >1 variables in file name
                destination = case Regex.captures(%r/.*\#\{(?<key>.+)\}.*/g, baseFile) do
                    nil        -> path <> baseFile
                    [key: key] -> path <> Regex.replace(%r/\#\{.+\}/, baseFile, HashDict.get(vars, key))
                end
                dir = :filename.dirname(destination)
                if !:filelib.is_dir(dir), do: create_directory dir
                cp_r file, destination, fn _, _ -> true end
                {:ok, origin} = read destination
                compiled = (
                    case Enum.uniq(Regex.scan(%r/\#\{([^\}]+)\}/, origin)) do
                        []   -> origin
                        data -> replacer(origin, vars, data)
                    end
                )
                write destination, compiled, []
            end
        end

        defp replacer(text, vars, []) do
            text
        end
        defp replacer(text, vars, [[entry, key] | tail]) do
           replacer(String.replace(text, entry, HashDict.get(vars, key), []), vars, tail)
        end

    end

end