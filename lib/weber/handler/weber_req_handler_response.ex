defmodule Handler.WeberReqHandler.Response do
 
  def handle_request({_type, status, data, headers}, req, state) do
    {:ok, req2} = :cowboy_req.reply(status, headers, data, req)
    {:ok, req2, state}
  end

end