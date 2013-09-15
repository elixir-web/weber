Weber
========

Weber - is a MVC Web framework for [Elixir](http://elixir-lang.org/). 

**Attention in very early stage**

## Quick start

 * Get and install Elixir from master.
 * Clone this repository.
 * Execute `mix deps.get` in the weber directory.
 * Execute `mix compile` in the weber directory.
 * Create new project with: `mix weber /home/user/testWeberApplication`

Now go to the `/home/user/testWeberApplication` and execute there: `mix deps.get && mix compile`. Make `sudo chomd +x ./start.sh` and after it you can try to run your testWeberApplication with:

```
./start.sh
```

and go to the http://localhost:8080

For more details see in `examples` directory.

## Directory structure

| Dir/File              | Description                                               |
| --------------------- |:---------------------------------------------------------:| 
|    ./start.sh         | Startup script                                            |
|    ./lib/controllers  | Directory with web controllers                            |
|    ./lib/helpers      | Helper functions                                          |
|    ./lib/models       | Directory for models (ecto)                               |
|    ./lib/static       | Directory for static files (css, js ....)                 |
|    ./lib/views        | Directory with EEx views                                  |
|    ./lib/app.ex       | Application startup settings                              |
|    ./lib/config.ex    | Configuration file.                                       |
|    ./lib/route.x      | File with routes declaration                              |

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
        {:render, [project: "simpleTodo"]}
    end

    def add("POST", [binding: body]) do
        new(body)
        {:json, [response: "ok"]}
    end

end
```

Every controller's action passes 2 parametes:

  * Http method
  * List of url bindings  

Controller can returns:

  * `{:render, [project: "simpleTodo"]}` - Render views with the same name as controller and sends it to response.
  * `{:json, [response: "ok"]}` - Weber convert keyword to json and sends it to response.

## Dependencies

  * [cowboy](https://github.com/extend/cowboy)
  * [ecto](https://github.com/elixir-lang/ecto)

## Contributing

  * Fork current repository
  * Make your changes
  * Send pull request

## Author

[@0xAX](https://twitter.com/0xAX).