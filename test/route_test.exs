defmodule WeberRouteTest do
  use ExUnit.Case

  import Weber.Route

  test "Test for Weber.Route.on and Weber.Route.otherwise" do
    r = on("/", :Controller1, :main_action)
      |> on("/user/0xAX/add", :Controller1, :action2)
      |> on("/user/:user/delete", :Controller1, :action2)
      |> otherwise("404", :Controller1, :notfound)
    
    assert(r == [[path: "/", controller: :Controller1, action: :main_action], 
                 [path: "/user/0xAX/add", controller: :Controller1, action: :action2], 
                 [path: "/user/:user/delete", controller: :Controller1, action: :action2], 
                 [path: "404", controller: :Controller1, action: :notfound]
                ])
  end

  test "Test for Weber.Route.match_routes_helper" do
    r = on("/", :Controller1, :main_action)
      |> on("/user/0xAX/add", :Controller1, :action2)
      |> on("/user/:user/delete", :Controller1, :action2)
      |> otherwise("404", :Controller1, :notfound)
    
    assert match_routes("/user/0xAX", r) == []
    assert match_routes("/", r) == [[path: "/", controller: :Controller1, action: :main_action]]
    assert match_routes("/user/0xAX/add/user2", r) == []
    assert match_routes("/user/0xAX/add/", r) == [[path: "/user/0xAX/add", controller: :Controller1, action: :action2]]

  end

end