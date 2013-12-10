defmodule Weber.Utils do
  
  @moduledoc """
    Weber utils functions.
  """

  import Enum

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
    filter(abs_filenames, fn({bname, _mod, _file}) ->
      bname == filename
    end) |> head
  end

  def find_static_file_path(abs_filenames, filename) do
    filter(abs_filenames, fn(file) ->
      Path.basename(file) == filename
    end)
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
    "<% import Weber.Helper.Html %>" <> "<% import Weber.Helper.IncludeView %>" <> 
    "<% import Weber.Helper.ResourceHelper %>" <> "<% import Weber.I18n %>" <> view_content
  end

  def views(path) do
    Enum.filter(get_all_files(:erlang.binary_to_list(path) ++ '/lib/views/'), 
      fn(f) -> :filename.extension(f) == '.html' 
    end)
  end

  @doc """
  Build module name from view path.

  Path: /home/user/testProj/lib/views/main.html
  Module: Elixir.Views.Main
  """
  def build_module_name(path) do
     spliten_path = case is_binary(path) do
      true -> String.split(path, "/")
      false -> String.split(:erlang.list_to_binary(path), "/")
     end
     drop_path = :lists.dropwhile(fn(segment) -> segment !== <<"views">> end, spliten_path)
     Enum.map(drop_path, fn(p) -> String.capitalize(:filename.basename(p, '.html')) end) |> Module.concat
  end

  def to_bin(val) when is_binary(val) do
    val
  end

  def to_bin(val) when is_atom(val) do
    atom_to_binary(val)
  end

  def to_bin(val) when is_list(val) do
    :erlang.list_to_binary(val)  
  end

  def to_bin(val) when is_integer(val) do
    integer_to_binary(val)
  end

  def to_bin(val) when is_float(val) do
    float_to_binary(val)  
  end

end