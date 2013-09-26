#!/usr/bin/env sh

if [ ! -f deps ]; then
  mix deps.get && mix compile
fi

export ERL_LIBS="$ERL_LIBS:#{path}"
exec iex -S mix
