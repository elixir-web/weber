Weber
========

Weber - is a MVC Web framework for [Elixir](http://elixir-lang.org/).

![weber-256](https://f.cloud.github.com/assets/197979/1323435/8403dbfe-347f-11e3-97b8-5f6bd1d902ca.png)

## Join the Community

  [`#WeberMVC` on freenode IRC](http://webchat.freenode.net/?channels=%23webermvc&uio=d4)

[![Build Status](https://travis-ci.org/0xAX/weber.png)](https://travis-ci.org/0xAX/weber)

## Features

 * MVC web framework;
 * Project generation;
 * Json generation with exjson;
 * Websocket support;
 * HTML helpers;
 * Web controller Helpers.
 * i18n support;
 * Sessions support;

## Quick start

 1. Get and install Elixir from master.
 2. Clone this repository.
 3. Execute `mix deps.get` in the weber directory.
 4. Execute `mix compile` in the weber directory.
 5. Execute `MIX_ENV=test mix do deps.get, test`
 6. Create new project with: `mix weber.new /home/user/testWebApp`

Now go to the `/home/user/testWebApp` and execute there: `mix deps.get && mix compile`. Then you can try to run your testWeberApplication with:

```
./start.sh
```

and go to the [http://localhost:8080/](http://localhost:8080/)

For more details see in `examples` directory and Weber's [API](http://0xax.github.io/weber/public/docs/index.html).

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
    route on("GET", "/", :Simpletodo.Main, :action)
      |> on("POST", "/add/:note", :Simpletodo.Main, :add)
```

Also `on` supports following syntax:

```elixir
    route on("GET", "/", "Simpletodo.Main#action")
      |> on("POST", "/add/:note", "Simpletodo.Main#add")
```

It is `route` macro which value is chain of `on` and `otherwise` functions with 3 parametes:
  
  * Http method
  * Route path, can be binding (starts with ':' symbol);
  * Module name of controller;
  * Function name from this controller.

Http method can be:

  * `"GET"`
  * `"POST"`
  * `"PUT"`
  * `"DELETE"`
  * `"ANY"`

## Controllers

Every Weber's controller is just an elixir module, like:

```elixir
defmodule Simpletodo.Main do

  import Simplemodel

  def action(_) do
    {:render, [project: "simpleTodo"], []}
  end

  def add([body: body]) do
    new(body)
    {:json, [response: "ok"], [{"Content-Type", "application/json"}]}
  end

end
```

Every controller's action passes 2 parameters:

  * HTTP method
  * List of URL bindings

Controller can return:

  * `{:render, [project: "simpleTodo"], [{"HttpHeaderName", "HttpHeaderValheaderVal"}]}` - Render views with the same name as controller and sends it to response.
  * `{:render_inline, "foo <%= bar %>", [bar: "baz"]}, []}` - Render inline template.
  * `{:file, path, headers}` - Send file in response.
  * `{:json, [response: "ok"], [{"HttpHeaderName", "HttpHeaderValheaderVal"}]}` - Weber convert keyword to json and sends it to response.
  * `{:redirect, "/main"}` - Redirect to other resource.
  * `{:text, data, headers}` - Sends plain text.
  * `{:nothing, ["Cache-Control", "no-cache"]}` - Sends empty response with status `200` and headers.

## Request params

Sometimes it is necessary for the request parameters in the controller. For this point can be used `Weber.Http.Params` [API](https://github.com/0xAX/weber/wiki/Weber.Http.Params-API).

```elixir
defmodule Simplechat.Main.Login do

  import Weber.Http.Params

  def render_login([]) do
    # get body request
    body = get_body()
    #
    # Do something with param
    #
    {:render, [project: "SimpleChat"], []}
  end

end
```

If you need to get parameters from query string, it is easy to do with `param/1` API. For example you got request for: `/user?name=0xAX`, you can get `name` parameter's value with:

```elixir
defmodule Simplechat.Main.Login do

  import Weber.Http.Params

  def render_login([]) do
    name = param(:name)
    #
    # Do something with param
    #
    {:render, [project: "SimpleChat", name: name], []}
  end

end
```

You can find the full API at the [wiki](https://github.com/0xAX/weber/wiki/Weber.Http.Params-API).

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

You can include your static resources like `javascript`, `css`, `favicon` or `image` files with resource helpers:

```elixir
#
# Generates: <script type="text/javascript" src="/static/test.js"></script>
script("/static/test.js")
# If no value is passed for src it defaults to "/public/application.js"
script()

#
# Generates: <link href="/static/test.css" rel="stylesheet" media="screen">
#
style("/static/test.css")
# If no value is passed for href it defaults to "/public/application.css"
style()

#
# Generates: <link href="/public/img/favicon.ico" rel="shortcut icon" type="image/png">
favicon("/public/img/favicon.ico")
# If no value is passed for href it defaults to "/public/img/favicon.ico"
favicon()

#
# Generates: <img src="/public/img/example.jpg" alt="Image" class="some-class" height="100" width="100">"
image("/public/img/example.jpg", [alt: "Image", class: "some-class", height: 100, width: 100])
```

## Controller Helpers

#### `content_for_layout` and `layout`

Create `layouts` folder in `views/` in inside put:

```HTML
<%= content_for_layout %>
```

Declare `layout` helper in your controller:

```elixirlang
defmodule TestController.Main do

  require Weber.Controller

  Weber.Controller.layout 'layout.html'
  
  ....        
end

```

template from current view will render in `layout.html` instead `<%= content_for_layout %>`

## Internationalization

There is `t` helper for Weber probject internationalization. Create lang file with name like `en_US` in `lang` directory:

```
{
  "HELLO_STR" : "Hello, It is weber framework!", 
  "FRAMEWORK_DESCRIPTION" : "Weber - is a MVC Web framework for Elixir."
}
```

and you can use it like:

```html
<span><%= t "HELLO_STR" %></span>
```

in your html template.

## Websocket

You can handle websocket connection and incoming/outcoming websocket message in your controllers.

First of all you need to designate websocket controller in your `config.ex` file in `webserver:` section, like:

```elixir
ws:
  [ws_port: 8800,
   ws_mod: :Handler
  ]
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

## Session

[Session API](https://github.com/0xAX/weber/wiki/Weber.Session-API)

## Dependencies

  * [cowboy](https://github.com/extend/cowboy)
  * [ecto](https://github.com/elixir-lang/ecto)
  * [exjson](https://github.com/guedes/exjson)
  * [mimetypes](https://github.com/spawngrid/mimetypes)

## Contributing

See [Contributing.md](https://github.com/0xAX/weber/blob/master/Contributing.md)

## Author

[@0xAX](https://twitter.com/0xAX).
