defmodule WeberRouteTest do
  use ExUnit.Case

  import Weber.Route

  test "Test for Weber.Route.on and Weber.Route.otherwise" do
    r = on("ANY", "/", :Controller1, :main_action)
      |> on("GET", "/user/0xAX/add", :Controller1, :action2)
      |> on("POST", "/user/:user/delete", :Controller1, :action2)
      |> otherwise("ANY", "404", :Controller1, :notfound)
    
    assert(r == [[method: "ANY", path: "/", controller: :Controller1, action: :main_action], 
                 [method: "GET", path: "/user/0xAX/add", controller: :Controller1, action: :action2], 
                 [method: "POST",path: "/user/:user/delete", controller: :Controller1, action: :action2], 
                 [method: "ANY", path: "404", controller: :Controller1, action: :notfound]
                ])
  end

  test "Test for Weber.Route.match_routes_helper" do
    r = on("ANY", "/", :Controller1, :main_action)
      |> on("POST", "/user/0xAX/add", :Controller1, :action2)
      |> on("POST", "/user/:user/delete", :Controller1, :action2)
      |> otherwise("ANY", "404", :Controller1, :notfound)
    
    assert match_routes("/user/0xAX", r, "POST") == []
    assert match_routes("/", r, "ANY") == [[method: "ANY", path: "/", controller: :Controller1, action: :main_action]]
    assert match_routes("/user/0xAX/add/user2", r, "GET") == []
    assert match_routes("/user/0xAX/add/", r, "POST") == [[method: "POST", path: "/user/0xAX/add", controller: :Controller1, action: :action2]]

    assert match_routes("/user/0xAX/add?role=admin", r, "POST") == [[method: "POST", path: "/user/0xAX/add", controller: :Controller1, action: :action2]]
    assert match_routes("/user/0xAX/delete?role=admin", r, "POST") == [[method: "POST", path: "/user/:user/delete", controller: :Controller1, action: :action2]]
    assert match_routes("/user/0xAX/remove?role=admin", r, "ANY") == []

  end

end