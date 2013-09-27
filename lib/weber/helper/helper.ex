defmodule Weber.Helper do
  @moduledoc """
  This module provides helpers functions to help you in views.
  """

  @doc """
  Returns the view `file`.
  You can use include_view inside your views.

  Usage: 
  include_view("path/file")
  Or
  include_view("path/file", [value: value])

  If you need files outside views path, use as: include_view("/complete/path/file") 
  """
  def include_view(file, args // []) do
    views_path = :gen_server.call(Mix.Project.get, :views)
    file = file |> Path.expand(views_path)

    EEx.eval_file(file, args)
  end
end
