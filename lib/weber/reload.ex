defmodule Weber.Reload do
  use GenServer.Behaviour

  defmodule Config do
    defstruct root_path: nil, load_modules: [], load_time: { { 1970, 1, 1 }, { 0, 0, 0 } }
  end

  def start() do
    __MODULE__.start_link(Weber.Path.__root__)
  end

  def start_link(root_path) do
    :gen_server.start({ :local, __MODULE__ }, __MODULE__, %Config{root_path: root_path}, [])
  end

  def init(config) do
    { :ok, config }
  end

  def handle_call(:purge_modules, _from, %Config{root_path: _root_path, load_modules: load_modules, load_time: load_time} = config) do
    paths = Path.wildcard(Weber.Path.__root__ <> "/lib/" <> "/**/*.ex") ++ Path.wildcard(Weber.Path.__root__ <> "/lib/" <> "/**/*.html")
    last_file_update = Enum.reduce(paths, load_time, &(last_file_reload_time(&1, &2)))
    if load_time == last_file_update do
      {:reply, :ok, config}
    else
      purge_modules(load_modules)
      Code.unload_files(paths)
      load_module()
      {:reply, :purged, config.load_modules([]).load_time(last_file_update)}
    end
  end

  def enable do
    Process.put(:elixir_ensure_compiled, true)
    Process.flag(:error_handler, Weber.Reload.ErrorHandler)
    :ok
  end

  def purge do
    case :erlang.whereis(__MODULE__) do
      :undefined ->
        File.cwd! |> __MODULE__.start_link()
      _->
        :pass
    end
    :gen_server.call(__MODULE__, :purge_modules)
  end

  defp load_module() do
    Path.wildcard(Weber.Path.__root__ <> "/lib/" <> "/**/*.ex") |> try_to_compile
  end

  defp last_file_reload_time(file, load_time) do
    case File.stat(file) do
      { :ok, %File.Stat{mtime: mtime} } -> max(mtime, load_time)
      { :error, _ } -> load_time
    end
  end

  defp purge_modules(modules) do
    Enum.each modules, fn(mod) ->
      :code.purge(mod)
      :code.delete(mod)
    end
  end

  defp try_to_compile([]) do
    []
  end

  defp try_to_compile([path | t]) do
    try do
      Kernel.ParallelCompiler.files([path], [])
      try_to_compile(t)
    catch
      kind, reason ->
        :erlang.raise(kind, reason, System.stacktrace)
        try_to_compile(t)
    end
  end

end