#!/usr/bin/env sh

export ERL_LIBS="$ERL_LIBS:/home/shk/simpleTodo"
exec iex -S mix
