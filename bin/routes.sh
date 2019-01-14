#!/bin/bash

getto_elm_routes_main(){
  local dump
  local render
  local build
  local target
  local json
  local info
  local path
  local module
  local entry_point

  dump=./node_modules/.bin/dump-routes-info
  render=./node_modules/.bin/render-ejs
  build=./bin/build.sh

  target=$1/$2

  echo render files...
  touch tmp/wip

  for json in $("$dump" -f "$target"); do
    info=$(echo "$json" | sed -n 's/^.*"info":{\([^}]\+\)}.*$/\1/p')
    path=$(echo "$info" | sed -n 's/^.*"path":"\([^"]\+\)".*$/\1/p')
    module=$(echo "$info" | sed -n 's/^.*"module":"\([^"]\+\)".*$/\1/p')
    module=${module//./\/}

    echo $path

    "$render" -d "$json" -t config/templates/html.ejs   -f public/dist/${path}.html
    "$render" -d "$json" -t config/templates/config.ejs -f public/dist/_config/${path}.js
    "$render" -d "$json" -t config/templates/elm.ejs    -f src/GettoUpload/App/${module}/EntryPoint.elm
  done

  sleep 1
  echo

  rm tmp/wip

  for entry_point in $(find modules/app/src -name EntryPoint.elm); do
    "$build" "./src" "${entry_point#modules/app/src/}"
  done
}

getto_elm_routes_main "$@"
