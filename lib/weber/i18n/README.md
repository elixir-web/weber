### Weber i18n

Weber supports now:

  * custom time localization
  * current utc time localization

Every localization file is a simple `json` file like:

```javascript
{
 "en_US" : {
    "datetime" :{
        "abbr_day_names" : ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
        "abbr_month_names" : ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                              "Aug", "Sep", "Oct", "Nov", "Dec"],
        "day_names" : ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                       "Friday", "Saturday"],
        "month_names" : ["January", "February", "March", "April", "May", "June", 
                         "July", "August", "September", "October", "November","December"],
        "format" : "%Y %B %d"
    }
 }
}
```

Date/Time formats:

  * `%Y` - year and century (2013)
  * `%y` - only century (13)
  * `%C` -     century (year divided by 100)
  * `%B` -     the full name of the month (October)
  * `%b` -     abbreviated name of the month (Oct)
  * `%m` -     month number
  * `%A` -     name of the day (Friday)
  * `%a` -     abbreviated name of the day (Fri)
  * `%d` -     day of the month
  * `%w` -     day of the 
  * `%H` -     hour
  * `%M` -     minute
  * `%S` -     second

### Date/Time localization

```elixir
defmodule MainController do

  import Weber.I18n

  def action("GET", conn) do
    {:render, [time: localize_time_now_utc(conn)], []}
  end

end
```

There are API for time localization in `Weber.I18n`:

  * localize_time_now_utc/1
  * localize_time_now/2

### Application internationalization

There is `t` helper for Weber probject internationalization. Create lang file with name like `en_US` in `lang` directory:

```
{
  "HELLO_STR" : "Hello, It is weber framework!", 
  "FRAMEWORK_DESCRIPTION" : "Weber - is a MVC Web framework for Elixir."
}
```

and you can use it like:

```html
<span><%= t(@conn, "HELLO_STR") %></span>
```

in your html template.