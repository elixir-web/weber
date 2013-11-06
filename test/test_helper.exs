Mix.start
Mix.env(:test)
Mix.shell(Mix.Shell.Process)
System.put_env("MIX_ENV", "test")
:application.start(:hackney)

#Code.require_file "support/config.exs", __DIR__
#Code.require_file "weber_fake/route.exs", __DIR__

ExUnit.start

defmodule MixHelpers do
  import ExUnit.Assertions

  def tmp_path do
    Path.expand("../../tmp", __FILE__)
  end

  def tmp_path(extension) do
    Path.join tmp_path, extension
  end

  def assert_file(file) do
    assert File.regular?(file), "Expected #{file} to be a file."
  end

  def assert_directory(dir) do
    assert File.dir?(dir), "Expected #{dir} to be a directory."
  end
 end