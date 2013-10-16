defmodule Handler.WeberReqHandler.Response do
  def handle_request({_type, status, headers}, req) do
    :cowboy_req.reply(status, headers, <<"">>, req)
  end

  def handle_request({_type, status, data, headers}, req) do
    :cowboy_req.reply(status, headers, data, req)
  end
end