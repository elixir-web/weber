defmodule Weber.Helper.IncludeView do
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

  If you need files outside views path, use as: 

      include_view("/complete/path/file") 
  """
  def include_view(file, args \\ []) do
     Path.expand(file, Weber.Path.__root__ <> "/lib/views/") |> EEx.eval_file(args)
  end
end
