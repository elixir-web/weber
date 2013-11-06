defmodule Simplechat do

  def start(_type, _args) do
    Room.start_link
    Weber.run_weber
  end

  def stop(_state) do
    :ok
  end
  
end