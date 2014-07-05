defmodule Weber.Utils do

  @moduledoc """
    Weber utils functions.
  """

  import Enum

  @doc """
    Convert :calendar.local_time to string
  """
  def get_time() do
    {{year, month, day}, {hours, minutes, seconds}}  = :calendar.local_time()
    Integer.to_string(year) <> "." <> Integer.to_string(month) <> "." <> Integer.to_string(day) <> " " <>
    Integer.to_string(hours) <> ":" <> Integer.to_string(minutes) <> ":" <> Integer.to_string(seconds) <> " "
  end

  @doc """
    Recursively get all files from directory.
  """
  def get_all_files(dir) do
    find_files = fn(f, acc) -> [f | acc] end
    :filelib.fold_files(dir, ".*", true, find_files, [])
  end

  @doc """
    Find full path by file name
  """
  def find_file_path(abs_filenames, filename) do
    filter(abs_filenames, fn(f) ->
      case f do
        {bname, _mod, _file} ->
          bname == filename
        _ ->
          Path.absname(f) == filename
      end
    end) |> head
  end

  def find_static_file_path(abs_filenames, filename) do
    filter(abs_filenames, &( Path.basename(&1) == List.to_string(filename) ) )
  end

  @doc """
  Return first element from list
  """
  def head([]), do: []
  def head([h | _]), do: h

  @doc """
  Collect all Helpers imports.
  """
  def add_helpers_imports(view_content) do
    "<% import Weber.Helper.Html %>" <> "<% import Weber.Helper.Partial %>" <>
    "<% import Weber.Helper.ResourceHelper %>" <> "<% import Weber.I18n %>" <> view_content
  end

  def views(path) do
    Enum.filter( get_all_files(:erlang.binary_to_list(path) ++ '/lib/views/'), &(Path.extname(&1) == '.html') )
  end

  @doc """
  Build module name from view path.

  Path: /home/user/testProj/lib/views/main.html
  Module: Elixir.Views.Main
  """
  def build_module_name(path) do
     spliten_path = path |> to_string |> String.split("/")
     drop_path = :lists.dropwhile(fn(segment) -> segment !== <<"views">> end, spliten_path)
     Enum.map(drop_path, &( Weber.Utils.capitalize(Path.basename(&1, '.html')) ) ) |> Module.concat
  end

  @doc """
  Capitalize only first character in string
  """
  def capitalize("") do
    ""
  end

  def capitalize(str) do
    (str |> String.at(0) |> String.capitalize) <> String.slice(str, 1..-1)
  end

  def to_bin(val) do
    to_string(val)
  end

end
