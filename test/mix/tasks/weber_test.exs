defmodule Mix.Tasks.WeberTest do
  use ExUnit.Case

  import MixHelpers 

  test "prints version" do
    Mix.Tasks.Weber.run ["--version"]
    assert_received { :mix_shell, :info, ["Weber v" <> _] }
  end

  test "generates a new weber app given an absolute path" do
    app_name = "new_app"
    app_module = Mix.Utils.camelize(app_name)
    app_path = tmp_path app_name
    File.rm_rf! app_path
    
    Mix.Generator.create_directory tmp_path

    Mix.Tasks.Weber.New.run [app_path]
    assert_directory app_path

    File.cd! app_path, fn ->
      assert_file "README.md"
      assert_file "mix.exs"
      assert_file "start.sh"

      assert_directory "lib"
      assert_directory "lib/helpers"
      assert_directory "lib//models"
      assert_file "lib/app.ex"
      assert_file "lib/config.ex"
      assert_file "lib/route.ex"
      assert_file "lib/views/Main.html"
      assert_file "lib/controllers/main.ex"
      assert_directory "logs"
      assert_directory "public/css"
      assert_directory "public/js"
      assert_directory "public/img"
      assert_file "test/#{app_name}_test.exs"
      assert_file "test/test_helper.exs"
      assert_directory "tmp"

      File.read!("lib/controllers/main.ex") =~ %r(defmodule #{app_module}.Main do)
      File.read!("lib/controllers/main.ex") =~ %r(project: "#{app_name}")
      File.read!("test/#{app_name}_test.exs") =~ %r(defmodule #{app_module}Test do)
    end

    File.rm_rf! app_path
  end
end
