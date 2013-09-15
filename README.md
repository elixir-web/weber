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

## Controllers  

## Dependencies

  * [cowboy](https://github.com/extend/cowboy)
  * [ecto](https://github.com/elixir-lang/ecto)

## Contributing

  * Fork current repository
  * Make your changes
  * Send pull request

## Author

[@0xAX](https://twitter.com/0xAX).