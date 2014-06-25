defmodule Weber.Localization.LocalizationManager do

  use GenServer.Behaviour

  defmodule LocalizationConfig do
    defstruct config: nil,
              default_locale: nil
  end

  def start_link(config) do
    :gen_server.start_link({:local, :localization_manager}, __MODULE__, [config], [])
  end

  def init([config]) do
    :gen_server.cast(:erlang.self(), :load_localization_files)
    { :ok, %LocalizationConfig{config: config}}
  end

  def handle_cast(:load_localization_files, state) do
    case :lists.keyfind(:localization, 1, state.config) do
      false ->
        {:stop, :normal, state}
      {:localization, localization_config} ->
        project_path = File.cwd!()

        default_locale = Keyword.fetch!(localization_config, :default_locale)
        use_locales = Keyword.fetch!(localization_config, :use_locales)
                      |> Enum.map(fn(l) -> Atom.to_string(l) <> ".json" end)

        on_files_in_path(
                         Path.join([project_path, "/deps/weber/lib/weber/i18n/localization/locale"]),
                         &( :lists.member(&1, use_locales) ),
                         &( Weber.Localization.Locale.start_link(String.to_atom(&1), &2) )
                        )

        on_files_in_path(
                         Path.join([project_path, "/lang"]),
                         fn (_) -> true end,
                         &( Weber.Translation.Translate.start_link(String.to_atom(&1), &2) )
                        )

        {:noreply, %LocalizationConfig{config: state.config, default_locale: default_locale}}
    end
  end

  def on_files_in_path(path, check_fun, apply_fun) do
    case File.ls(path) do
      {:ok, files} ->
        Enum.each(files, &( check_fun.(&1) && apply_on_file(path, &1, apply_fun) ) )
      _ -> false
    end
  end

  def apply_on_file(path, file, apply_fun) do
    file_data = File.read!(Path.join([path, file]))
    (file_data != <<>>) && apply_fun.(file, file_data)
  end

end
