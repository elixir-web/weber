defmodule Weber.Localization.LocalizationManager do
  
  use GenServer.Behaviour
  
  defrecord LocalizationConfig,
    config: nil,
    default_locale: nil

  def start_link(config) do
    :gen_server.start_link({:local, :localization_manager}, __MODULE__, [config], [])
  end

  def init([config]) do
    :gen_server.cast(:erlang.self(), :load_localization_files)
    { :ok, LocalizationConfig.new config: config}
  end

  def handle_cast(:load_localization_files, state) do
    case :lists.keyfind(:localization, 1, state.config) do
      false ->  
        {:stop, :normal, :shutdown_ok, state}
      {:localization, localization_config} ->
        {_, default_locale}  = :lists.keyfind(:default_locale, 1, localization_config)
        {:ok, project_path} = File.cwd()
        {:ok, localization_files} = File.ls(project_path <> "/deps/weber/lib/weber/i18n/localization/locale")

        Enum.each(localization_files, 
          fn (file) ->
            {:ok, data} = File.read(project_path <> "/deps/weber/lib/weber/i18n/localization/locale/" <> file)
            Weber.Localization.Locale.start_link(binary_to_atom(file), data)  
          end)
        {:noreply, LocalizationConfig.new config: state.config, default_locale: default_locale}
    end
  end

  def handle_cast({:set_current_locale, locale}, state) do
    {:noreply, LocalizationConfig.new config: state.config, default_locale: locale <> ".json"}
  end

  def handle_call(:get_current_locale, _from, state) do
    {:reply, :lists.nth(1, String.split(state.default_locale, ".")), state}
  end

end