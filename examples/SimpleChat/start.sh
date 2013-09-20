#!/usr/bin/env sh

if [ ! -f deps ]; then
    mix deps.get && mix compile
fi

export ERL_LIBS="$ERL_LIBS:/home/shk/SimpleChat"
exec iex -S mix
