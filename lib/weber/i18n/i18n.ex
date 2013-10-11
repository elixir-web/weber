defmodule Weber.I18n do

  import Weber.Time

  @doc """
  Set current locale
  """
  def set_current_locale(locale) do
    :gen_server.cast(:localization_manager, {:set_current_locale, locale})
  end

  @doc """
  Get current locale
  """
  def get_current_locale do
    :gen_server.call(:localization_manager, :get_current_locale)
  end

  @doc """
  Localize UTC time now
  """
  def localize_time_now_utc do
    {date_time_format, locale_process_name} = localize_time_helper
    format_time(date_time_format, locale_process_name)
  end

  def localize_time_now(datetime = {{_year, _month, _day}, {_hour, _minute, _second}}) do
    {date_time_format, locale_process_name} = localize_time_helper
    format_time(date_time_format, locale_process_name, datetime)
  end

  defp localize_time_helper do
    current_locale = :gen_server.call(:localization_manager, :get_current_locale)
    locale_process_name = binary_to_atom(current_locale <> ".json")
    date_time_format = :gen_server.call(locale_process_name, :get_date_time_format)
    {date_time_format, locale_process_name}
  end
    
end