defmodule Weber.Time do
  @moduledoc """
  Weber Time utils functions.
  """

  @doc """
  Return tuple with current date/time
  """
  def now do
    :calendar.now_to_datetime(:erlang.now)
  end
  
  def format_time(format, locale_pid, date = _datetime \\ now) do
    format_time(format, locale_pid, "", date)
  end

  def format_time(<<>>, _locale_pid, result, _) do
    result
  end

  # %
  def format_time(<<37, rest :: binary>>, locale_pid, result, datetime) do
    format_time(rest, locale_pid, result <> "", datetime)
  end

  # tab
  def format_time(<<9, rest :: binary>>, locale_pid, result, datetime) do
    format_time(rest, locale_pid, result <> "\t", datetime)
  end

  # \n
  def format_time(<<10, rest :: binary>>, locale_pid, result, datetime) do
    format_time(rest, locale_pid, result <> "\n", datetime)
  end
  
  # \r
  def format_time(<<13, rest :: binary>>, locale_pid, result, datetime) do
    format_time(rest, locale_pid, result <> "\r", datetime)
  end
  
  # ' '
  def format_time(<<32, rest :: binary>>, locale_pid, result, datetime) do
    format_time(rest, locale_pid, result <> " ", datetime)
  end

  # Y
  def format_time(<<89, rest :: binary>>, locale_pid, result, datetime = {{year, _, _}, _}) do
    format_time(rest, locale_pid, result <> integer_to_binary(year), datetime)
  end

  # y
  def format_time(<<121, rest :: binary>>, locale_pid, result, datetime = {{year, _, _}, _}) do
    format_time(rest, locale_pid, result <> integer_to_binary(year - 2000), datetime)
  end

  # C
  def format_time(<<67, rest :: binary>>, locale_pid, result, datetime = {{year, _, _}, _}) do
    format_time(rest, locale_pid, result <> integer_to_binary(trunc (year / 100) + 1), datetime)
  end

  # B
  def format_time(<<66, rest :: binary>>, locale_pid, result, datetime = {{_, month, _}, _}) do
    monthes = :gen_server.call(locale_pid, :get_month_names)
    format_time(rest, locale_pid, result <> :lists.nth(month, monthes), datetime)
  end

  # b
  def format_time(<<98, rest :: binary>>, locale_pid, result, datetime = {{_, month, _}, _}) do
    monthes = :gen_server.call(locale_pid, :get_abbr_month_names)
    format_time(rest, locale_pid, result <> :lists.nth(month, monthes), datetime)
  end

  # m
  def format_time(<<109, rest :: binary>>, locale_pid, result, datetime = {{_, month, _}, _}) do
    format_time(rest, locale_pid, result <> integer_to_binary(month), datetime)
  end

  # A
  def format_time(<<65, rest :: binary>>, locale_pid, result, datetime = {date, _}) do
    day_num = :calendar.day_of_the_week(date)
    days = :gen_server.call(locale_pid, :get_day_names)
    format_time(rest, locale_pid, result <> :lists.nth(day_num, days), datetime)
  end

  # a
  def format_time(<<97, rest :: binary>>, locale_pid, result, datetime = {date, _}) do
    day_num = :calendar.day_of_the_week(date)
    days = :gen_server.call(locale_pid, :get_abbr_day_names)
    format_time(rest, locale_pid, result <> :lists.nth(day_num, days), datetime)
  end

  # d 
  def format_time(<<100, rest :: binary>>, locale_pid, result, datetime = {{_, _, day}, _}) do
    format_time(rest, locale_pid, result <> integer_to_binary(day), datetime)
  end

  # w
  def format_time(<<119, rest :: binary>>, locale_pid, result, datetime = {date, _}) do
    day_num = :calendar.day_of_the_week(date)
    format_time(rest, locale_pid, result <> integer_to_binary(day_num), datetime)
  end

  # H
  def format_time(<<72, rest :: binary>>, locale_pid, result, datetime = {_, {hour, _, _}}) do
    format_time(rest, locale_pid, result <> integer_to_binary(hour), datetime)
  end

  # M
  def format_time(<<77, rest :: binary>>, locale_pid, result, datetime = {_, {_, minute, _}}) do
    format_time(rest, locale_pid, result <> integer_to_binary(minute), datetime)
  end

  # S
  def format_time(<<83, rest :: binary>>, locale_pid, result, datetime = {_, {_, second, _}}) do
    format_time(rest, locale_pid, result <> integer_to_binary(second), datetime)
  end

  # '
  def format_time(<<39, rest :: binary>>, locale_pid, result, datetime) do
    format_time(rest, locale_pid, result, datetime)
  end

  def format_time(<<c, rest :: binary>>, locale_pid, result, datetime) do
    {:ok, s} = String.from_char_list([c])
    format_time(rest, locale_pid, result <> s, datetime)
  end

end