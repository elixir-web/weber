defmodule Weber.Localization.Locale do
  use GenServer.Behaviour

  defrecord Locale,
    locale: nil

  def start_link(name, locale) do
    :gen_server.start_link({:local, name}, __MODULE__, [locale], [])
  end

  def init([locale]) do
    { :ok, Locale.new locale: JSON.parse(locale)}
  end

  def handle_call(:get_abbr_day_names, _from, state) do
    {:reply, get_current_date_time_param("abbr_day_names", state.locale), state}
  end

  def handle_call(:get_abbr_month_names, _from, state) do
    {:reply, get_current_date_time_param("abbr_month_names", state.locale), state}
  end

  def handle_call(:get_day_names, _from, state) do
    {:reply, get_current_date_time_param("day_names", state.locale), state}
  end

  def handle_call(:get_month_names, _from, state) do
    {:reply, get_current_date_time_param("month_names", state.locale), state}
  end

  def handle_call(:get_date_time_format, _from, state) do
    {:reply, get_current_date_time_param("format", state.locale), state}  
  end

  def get_current_date_time_param(param_name, locale) do
    {_, locale_metrics} = :lists.keyfind("en_US", 1, locale)
    {_, datetime} = :lists.keyfind("datetime", 1, locale_metrics)
    {_, param} = :lists.keyfind(param_name, 1, datetime)
    param
  end

end