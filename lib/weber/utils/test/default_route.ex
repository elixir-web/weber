defmodule Weber.DefaultRoute do

	import Weber.Route
	require Weber.Route

	route on("GET", "/", :WeberTest.Main, :redirect_action)
	
end