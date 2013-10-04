Weber
========

Weber - is a MVC Web framework for [Elixir](http://elixir-lang.org/). 

**Attention in very early stage**

## Join the Community

  [`#WeberMVC` on freenode IRC](http://webchat.freenode.net/?channels=%23webermvc&uio=d4)

[![Build Status](https://travis-ci.org/0xAX/weber.png)](https://travis-ci.org/0xAX/weber)

## Features
 
 * MVC web framework;
 * Project generation;
 * Json generation with exjson;
 * Websocket support;

## Quick start

 1. Get and install Elixir from master.
 2. Clone this repository.
 3. Execute `mix deps.get` in the weber directory.
 4. Execute `mix compile` in the weber directory.
 5. Create new project with: `mix weber.new /home/user/testWebApp`

Now go to the `/home/user/testWebApp` and execute there: `mix deps.get && mix compile`. Then you can try to run your testWeberApplication with:

```
./start.sh
```

and go to the [http://localhost:8080/](http://localhost:8080/)

For more details see in `examples` directory.

## Directory structure

| Dir/File              | Description                                               |
| --------------------- |:---------------------------------------------------------:| 
|    ./start.sh         | Startup script                                            |
|    ./lib/controllers  | Directory with web controllers                            |
|    ./lib/helpers      | Helper functions                                          |
|    ./lib/models       | Directory for models (ecto)                               |
|    ./lib/views        | Directory with EEx views                                  |
|    ./lib/app.ex       | Application startup settings                              |
|    ./lib/config.ex    | Configuration file.                                       |
|    ./lib/route.ex     | File with routes declaration                              |
|    ./public           | Directory for static files (css, js ....)                 |

## Routing

Routing declaration is in `route.ex` files:

```elixir
    @route on("/", :Simpletodo.Main, :action)
        |> on("/add/:note", :Simpletodo.Main, :add)
```

It is `@route` attribute which value is chain of `on` and `otherwise` functions with 3 parametes:

  * Route path, can be binding (starts with ':' symbol);
  * Module name of controller;
  * Function name from this controller.

## Controllers

Every Weber's controller is just elixir module, like:

```elixir
defmodule Simpletodo.Main do

    import Simplemodel

    def action("GET", []) do
        {:render, [project: "simpleTodo"], []}
    end

    def add("POST", [body: body]) do
        new(body)
        {:json, [response: "ok"], [{"Content-Type", "application/json"}]}
    end

end
```

Every controller's action passes 2 parametes:

  * HTTP method
  * List of URL bindings  

Controller can returns:

  * `{:render, [project: "simpleTodo"], [{"HttpHeaderName", "HttpHeaderValheaderVal"}]}` - Render views with the same name as controller and sends it to response.
  * `{:json, [response: "ok"], [{"HttpHeaderName", "HttpHeaderValheaderVal"}]}` - Weber convert keyword to json and sends it to response.
  * `{:redirect, "/main"}` - Redirect to the another resource.
  * `{:text, data, headers}` - Sends plain text.
  * `{:nothing, ["Cache-Control", "no-cache"]}` - Sends empty response with status `200` and headers.

## Request params

Sometimes it is necessary for the request parameters in the controller. For this point can be used `Weber.Http.Params` API:

```elixir
defmodule Simplechat.Main.Login do

  import Weber.Http.Params

    def render_login("GET", []) do
      # get body request
      body = get_body()
      #
      # Do something with param
      #
      {:render, [project: "SimpleChat"], []}
    end

    def join("POST", [{"username", username}]) do
      {:json, [chat: username], [{"Content-Type", "application/json"}]}
    end

end
```

Full API you can find at the [wiki](https://github.com/0xAX/weber/wiki/Weber.Http.Params-API).

## Helper

### Html Helper
Html helpers helps to generate html templates from elixir:

```elixir
defmodule Simpletodo.Helper.MyHelper
  import Weber.Helper.Html
  
  # Generates <p>test</p>
  def do_something do
    tag(:p, "test")
  end

  # Generates <p class="class_test">test</p>
  def do_something do
    tag(:p, "test", [class: "class_test"])
  end

  # Generates <img src="path/to/file">
  def do_something do
    tag(:img, [src: "path/to/file"])
  end
end
```

Tags with blocks

```elixir
defmodule Simpletodo.Helper.MyHelper
  import Weber.Helper.Html
  
  # Generates <div id="test"><p>test</p></div>
  def do_something do
    tag(:div, [id: "test"]) do
      tag(:p, "test")
    end
  end
end
```

### Include view in your html
Include view helper helps to include other views inside another.

Import in your controller.

```elixir
import Weber.Helper
```

Your view.

```html
<p>Test</p>
<%= include_view "test.html", [value: "value"]%> 
```

### Resource Helpers

You can include your static resources like `javascript` or `favicon` files with resource helpers:

```elixir
#
# Generates: <script type="text/javascript" src="/static/test.js"></script>
#
script("/static/test.js")


#
# Generates: <link href="/static/test.css" rel="stylesheet" media="screen">
#
link("/static/test.css")
```

## Websocket

You can handle websocket connection and incoming/outcoming websocket message in your controllers.

First of all you need to designate websocket controller in your `config.ex` file in `webserver:` section, like:

```elixir
use_ws: true,
ws_port: 8800,
ws_mod: :Simplechat.Main.Chat 
```

After it you must implement 3 callbacks in your controller like this:

```elixir
defmodule Simplechat.Main.Chat do

    def websocket_init(pid) do
        #
        # new websocket connection init
        #
    end

    def websocket_message(pid, message) do
        #
        # handle incoming message here
        #
    end

    def websocket_terminate(pid) do
        #
        # connection terminated
        #
    end

end
```

## Dependencies

  * [cowboy](https://github.com/extend/cowboy)
  * [ecto](https://github.com/elixir-lang/ecto)
  * [exjson](https://github.com/guedes/exjson)
  * [mimetypes](https://github.com/spawngrid/mimetypes)

## Contributing

  * Fork current repository
  * Make your changes
  * Send pull request

## Author

[@0xAX](https://twitter.com/0xAX).
