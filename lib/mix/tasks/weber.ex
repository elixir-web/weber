defmodule Mix.Tasks.Weber do

  @moduledoc """

    Create a new weber project template.

    Usage:

      mix weber.new /home/user/myWebApp - Creates myWebApp directory with weber project skeleton.
      mix weber --version - Prints weber version.
  """

  @shortdoc "Create a new weber project"

  use Mix.Task
  
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
      mix weber --help    -- shows weber help
    """
  end

  defmodule New do

    @doc """
    Create default weber based project.
    """
    def run([directoryName]) do
      # create project directory
      case File.mkdir(directoryName) do
        :ok -> :ok
        err -> :io.format("[Weber error] Can't create project directory. Error: ~p~n", [err])
      end

      #
      # create project skeleton
      #
      path = Path.expand directoryName
      baseName = Path.basename directoryName
      
      vars = HashDict.new [
        {"path", path},
        {"projectNamespace", Mix.Utils.camelize(baseName)},
        {"projectName", baseName}
      ]

      template = "default"
      {:ok, pwd} = File.cwd
      skelRoot = pwd <> "/templates/" <> template
      File.cd skelRoot
      skelFiles = Weber.Utils.get_all_files(".")
      lc file inlist skelFiles do
        baseFile = String.slice(file, 1, 1024)
        destination = path <> replace(baseFile, vars)
        dir = :filename.dirname(destination)
        if !:filelib.is_dir(dir), do: Mix.Generator.create_directory dir
        File.cp_r file, destination, fn _, _ -> true end
        {:ok, origin} = File.read destination
        compiled = replace(origin, vars)
        File.write destination, compiled, []
      end
    end

    @doc """
    Create weber based project with utils
    """
    def run([directoryName | rest]) do
      run([directoryName])
      Enum.each(rest, 
        fn(addition) -> 
          case addition do
            "--grunt" -> add_grunt(directoryName)
            _ -> :wrong
          end
        end)
    end

    defp add_grunt(directoryName) do
      path = Path.absname directoryName
      basename = Path.basename(directoryName)
      Mix.Generator.create_file path <> <<"/package.json">>, (package_json basename)
      Mix.Generator.create_file path <> <<"/Gruntfile.js">>, (gruntfile)
    end

    defp package_json(basename) do
      """
      {
        "name": "#{basename}",
        "version": "0.0.1",
        "devDependencies": {
          "grunt": "~0.4.2"
        }
      }
      """
    end

    defp gruntfile do
      """
      module.exports = function(grunt) {
        grunt.initConfig({
          pkg: grunt.file.readJSON('package.json')
        });
      };
      """
    end

    defp replace(text, vars) do
      case Enum.uniq(Regex.scan(%r/\#\{([^\}]+)\}/, text)) do
        []   -> text
        data -> replace_act(text, vars, data)
      end
    end

    defp replace_act(text, _vars, []) do
      text
    end
    
    defp replace_act(text, vars, [[entry, key] | tail]) do
      replace_act(String.replace(text, entry, HashDict.get(vars, key), []), vars, tail)
    end

  end

end
