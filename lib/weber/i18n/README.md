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

### Usage

```elixir
defmodule MainController do

  import Weber.I18n

  def action("GET", []) do
    set_current_locale("de_DE")
    "de_DE" = get_current_locale
    {:render, [time: localize_time_now_utc], []}
  end

end
```

### Contribute to Weber i18n
  
  * Clone `Weber` to your workstation;
  * Add `json` file with your localization to `weber/lib/i18n/localization/locale`
  * Send pull request.