defmodule Weber.Utils do
  
  @moduledoc """
    Weber utils functions.
  """

  import Enum
  import Path

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
    filter(abs_filenames, fn(file) ->
      :io.format("basename(file) ~p~n", [basename(file)])
      :io.format("filename ~p~n", [filename])
      :io.format("== ~p~n", [basename(file) == filename])
      (basename(file) == filename)
    end)
  end

  @doc """
  Collect all Helpers imports.
  """
  def add_helpers_imports(view_content) do
    "<% import Weber.Helper.Html %>" <> "<% import Weber.Helper.IncludeView %>" <> 
    "<% import Weber.Helper.ResourceHelper %>" <> "<% import Weber.I18n %>" <> view_content
  end

  @doc """
  Build module name from view path.

  Path: /home/user/testProj/lib/views/main.html
  Module: Elixir.Views.Main
  """
  def build_module_name(path) do
     spliten_path = String.split(:erlang.list_to_binary(path), "/")
     drop_path = :lists.dropwhile(fn(segment) -> segment !== <<"views">> end, spliten_path)
     Enum.map(drop_path, fn(p) -> String.capitalize(:filename.basename(p, '.html')) end) |> Module.concat
  end

end