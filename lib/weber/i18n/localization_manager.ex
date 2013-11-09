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
        {:stop, :normal, state}

      {:localization, localization_config} ->
        {:ok, project_path} = File.cwd()
        {_, default_locale}  = :lists.keyfind(:default_locale, 1, localization_config)
        {_, use_locales}  = :lists.keyfind(:use_locales, 1, localization_config)
        use_locales = Enum.map(use_locales, fn(l) -> atom_to_binary(l) <> ".json" end)

        case File.ls(project_path <> "/deps/weber/lib/weber/i18n/localization/locale") do
          {:ok, localization_files} ->
            Enum.each(localization_files, fn (file) ->
              case :lists.member(file, use_locales) do
                true ->
                  case File.read(project_path <> "/deps/weber/lib/weber/i18n/localization/locale/" <> file) do
                    {:ok, data} -> 
                      Weber.Localization.Locale.start_link(binary_to_atom(file), data)  
                    _ -> :ok
                  end
                false ->
                    :ok
              end
            end)
          _ -> :ok
        end
        
        case File.ls(project_path <> "/lang") do
          {:ok, translation_files} ->
            Enum.each(translation_files, fn (file) ->
              {:ok, translation_file_data} = File.read(project_path <> "/lang/" <> file)
              case translation_file_data do
                <<>> -> :ok
                _ -> Weber.Translation.Translate.start_link(binary_to_atom(file), translation_file_data)
              end
            end)
          _ -> :ok
        end
                
        {:noreply, LocalizationConfig.new config: state.config, default_locale: default_locale}
    end
  end

end