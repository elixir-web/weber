Weber
========

Weber - is a MVC Web framework for [Elixir](http://elixir-lang.org/). 

**Attention in very early stage**

Quick start
===========

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

Dependencies
==============

  * [cowboy](https://github.com/extend/cowboy)
  * [ecto](https://github.com/elixir-lang/ecto)

Contributing
===============

  * Fork current repository
  * Make your changes
  * Send pull request

Author
========

[@0xAX](https://twitter.com/0xAX).