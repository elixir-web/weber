defmodule Weber.Utils do
	
	@moduledoc """
		Weber utils functions
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
      basename(file) == filename
    end)
  end

end