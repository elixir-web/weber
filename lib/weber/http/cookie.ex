defmodule Weber.Http.Cookie do
    
  def generate_session_id do
    term_to_binary({:erlang.now, :crypto.rand_uniform(0, 1000)})
  end

end