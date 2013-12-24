defmodule WeberUtilsTest do
  
  use ExUnit.Case

  import Weber.Utils

  test "`Weber.Utils.capitalize` test" do
  	assert capitalize("") == ""
  	assert capitalize("test") == "Test"
  	assert capitalize("testTest") == "TestTest"
  end

end