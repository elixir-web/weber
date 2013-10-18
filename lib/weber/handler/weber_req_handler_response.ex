defmodule Handler.WeberReqHandler.Response do
  def handle_request({_type, status, headers}, req, state) do
    :cowboy_req.reply(status, headers, <<"">>, req)
    {:ok, req, state}
  end

  def handle_request({_type, status, data, headers}, req, state) do
    :cowboy_req.reply(status, headers, data, req)
    {:ok, req, state}
  end
end