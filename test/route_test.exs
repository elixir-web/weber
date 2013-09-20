defmodule WeberRouteTest do
  use ExUnit.Case

  import Weber.Route
  import Weber.Http.Url

  test "Test for Weber.Route.on and Weber.Route.otherwise" do
      r = on("/", 'controller1', 'main_action')
          |> on("/user/0xAX/add", 'controller1', 'action2')
          |> on("/user/:user/delete", 'controller1', 'action2')
          |> otherwise(404, 'controller1', 'notfound')
    
      assert(r == [[path: "/", controller: 'controller1', action: 'main_action'], 
                   [path: "/user/0xAX/add", controller: 'controller1', action: 'action2'], 
                   [path: "/user/:user/delete", controller: 'controller1', action: 'action2'], 
                   [path: 404, controller: 'controller1', action: 'notfound']
                  ])
  end

  test "Test for Weber.Route.match_routes_helper" do
    
  end

end