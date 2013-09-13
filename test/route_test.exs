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
    test_url = getBinding("/user/0xAX/password/:password")
    test_url2 = getBinding("/user/0xAX/password/123")
    test = match_routes_helper(test_url, test_url2)
    assert(test == true)

    test_url = getBinding("/user/0xAX/password/:password")
    test_url2 = getBinding("/")
    test = match_routes_helper(test_url, test_url2)
    assert(test == false)

    test_url = getBinding("/user/:name/password/:password")
    test_url2 = getBinding("/user/0xAX/password/123")
    test = match_routes_helper(test_url, test_url2)
    assert(test == true)

    test_url = getBinding("/user/:name")
    test_url2 = getBinding("/user/0xAX")
    test = match_routes_helper(test_url, test_url2)
    assert(test == true)
  end

end