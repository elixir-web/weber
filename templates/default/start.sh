#!/usr/bin/env sh

if [ ! -f deps ]; then
  mix deps.get && mix compile
fi

exec iex -S mix
