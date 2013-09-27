Mix.start
Mix.env(:dev)
Mix.shell(Mix.Shell.Process)
System.put_env("MIX_ENV", "dev")

ExUnit.start
