simpleTodo
============

Simple todo web application built with [Weber](https://github.com/0xAX/weber).

Create `simpletodo` database and table:

```sql
CREATE TABLE todo (
	id SERIAL;
	note varchar(250);
);
```

and then execute:

  * `mix deps.get`
  * `./start.sh`

