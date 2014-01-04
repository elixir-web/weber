ExUnit.start

Code.require_file(File.cwd! <> "/test/support/config.exs")
Code.require_file(File.cwd! <> "/test/route.ex")
Code.require_file(File.cwd! <> "/test/controllers.ex")

defmodule MixHelpers do

  import ExUnit.Assertions

  :hackney.start()
  
  Mix.shell(Mix.Shell.Process)
  System.put_env("MIX_ENV", "test")
  
  def tmp_path do
    Path.expand("../tmp", __DIR__)
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

  require Weber.Templates.ViewsLoader

  # Set resources
  Weber.Templates.ViewsLoader.set_up_resources(File.cwd! <> "/test")
  # compile all views
  Weber.Templates.ViewsLoader.compile_views(File.cwd! <> "/test")
  # start weber application
  Weber.run_weber

 end
