ExUnit.start

System.put_env("MIX_ENV", "test")

defmodule MixHelpers do
  
  :hackney.start()

end