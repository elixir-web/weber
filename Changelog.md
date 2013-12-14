#### Weber-0.0.5 [In progress]

  * #112 fix. Compile now with `mix compile --all --force`
  * `render_other` in response added.
  * Added ability to test Weber based web applications
  * Running Weber in daemon mode.
  * `headers` not required field in response.
  * Initial `Grunt` integration.
  * #115. Reverse Routing.
  * #118. Redirect in routing.
  * Regular expression in the routing.
  * #122. `content_for_layout` fixed.
  * New github pages

#### Weber-0.0.4 [Released]

  * [Optimization works]
    * `get_all_files`, `get_root`, `get_views`, `get_static` moved to the macros.
    * compile all views to the modules at the compile time.
    * build views module names in compile time.
    * static files serving improved.
  * All views files must start with capital letter.
  * `Plug` added to the dependecies.
  * `use_internationalization` new config option.
  * `use_sessions` new config option.
  * `SimpleChat` adapted to the last `Weber`.
  * `favicon.ico` added to the `SimpleChat`.
  * `mimetypes` dependecie removed.
  * #104 fixed.
  * #81 fixed.
  * #102 closed. Convinience way to test weber.
  * `otherwise` clause removed from routing.
  * `redirect` crash fix.
  * `DefaultConfig` removed.

#### Weber-0.0.3 [Released]

  * Ability to create new weber project with relative path;
  * Removed `use_ws` option from config.
  * Replaced `link` with `style` in resource helper.
  * Added `image` resource helper.
  * Sessions support.
  * Added favicon.ico to the generated project.
  * `link` helper renamed to `style`.
  * Add all imports to the view before rendering.
  * `SimpleChat` updated. Made with user sessions.
  * HTTP handler refactored.
  * `content_for_layout` helper.
  * Controller Helpers.
  * `layout` controller helper.
  * Get rid of `get_route`, route macros instead.
  * `t` helper for text translation.
  * `Makefile` added.
  * HTTP method moved to the router.
  * audio html helper.
  * video html helper.
  * Controller can return {:render_other, "test.html", []}
  * HTTP and WebSocket handler at the same port.
  * `index.html` in new weber project.
  * generated locales from `CLDR` added
  * Initial internationalization support.

#### Weber-0.0.2 [Released]

  * HTTPS support;
  * `test` directory with tests template added to weber application;
  * Automatic `chmod` for `start.sh`;
  * `mimetypes` added to dependecies;
  * Redirect support;
  * Websocket support;
  * Binding error fixed. Now every controller getting second parameter as [binding_name: binding_val];
  * New config options in `webserver` section: `use_ws`, `ws_port`, `ws_mod`;
  * New example - SimpleTodo;
  * `weber.new` mix task instead of `weber`;
  * Creation directories for static files (`css`/`js`/`img`) at weber project initialization;
  * Added ability to send headers in controller's response;
  * HTML helpers;
  * [Weber at github pages](http://0xax.github.io/weber/index.html);
  * New directory [structure](https://github.com/0xAX/weber/wiki/Weber-project-directory-structure);
  * New `include_view` helper;
  * Resource helpers;
  * Empty response with headers support;
  * Plain text response support;
  * Rendering inline template support added;
  * Query string parameters;
  * Send file in response;
  * `controller#action` syntax support in router.

#### Weber-0.0.1 [Released]

  * Initial release.
