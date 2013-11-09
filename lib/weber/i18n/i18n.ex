defmodule Weber.I18n do

  import Weber.Time
  import Weber.Session

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
    date_time_format = :gen_server.call(binary_to_atom(get_session(:locale) <> ".json"), :get_date_time_format)
    {date_time_format, binary_to_atom(get_session(:locale) <> ".json")}
  end

  @doc """
  Translation helper.
  """
  def t(key) do
    case Weber.Session.get_session(:locale) do
      [] -> 
        :gen_server.call(:en_US, {:translate, key})
      locale ->
        :gen_server.call(locale, {:translate, key})
    end
  end
    
end