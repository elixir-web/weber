defmodule Weber.I18n do

  import Weber.Time
  import Weber.Session

  @doc """
  Localize UTC time now
  """
  def localize_time_now_utc(conn) do
    {date_time_format, locale_process_name} = localize_time_helper(conn)
    format_time(date_time_format, locale_process_name)
  end

  def localize_time_now(conn, datetime = {{_year, _month, _day}, {_hour, _minute, _second}}) do
    {date_time_format, locale_process_name} = localize_time_helper(conn)
    format_time(date_time_format, locale_process_name, datetime)
  end

  defp localize_time_helper(conn) do
    pid = case get_session(conn, :locale) do
      [] -> "en_US.json"
      l -> l <> ".json"
    end
    date_time_format = :gen_server.call(binary_to_atom(pid), :get_date_time_format)
    {date_time_format, binary_to_atom(pid)}
  end

  @doc """
  Translation helper.
  """
  def t(conn, key) do
    case Weber.Session.get_session(conn, :locale) do
      [] -> 
        :gen_server.call(:en_US, {:translate, key})
      locale ->
        :gen_server.call(locale, {:translate, key})
    end
  end
    
end