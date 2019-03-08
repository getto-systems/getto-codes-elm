#!/bin/bash

getto_elm_build_main(){
  local elm
  local elm_test
  local elm_cache
  local dist
  local target_dir
  local target
  local src_target
  local tmp_target
  local dist_target
  local test_target

  if [ -f tmp/wip ]; then
    return
  fi

  elm=./node_modules/.bin/elm
  elm_test=./node_modules/.bin/elm-test
  elm_cache=./elm-stuff/0.19.0

  dist=./public/dist/_app

  target_dir=$1; shift
  target=$1; shift

  src_target=src/${target}
  tmp_target=/tmp/elm/${target}.js
  test_target=tests/${target%.elm}Test.elm

  # Path/To/ElmModule.elm => path/to/elm_module.elm.js
  dist_target=$target
  dist_target=$(echo "$dist_target" | sed -r -e 's/^([A-Z])/\L\1\E/' -e 's/([A-Z])/_\L\1\E/g')
  dist_target=${dist_target//\/_/\/}
  dist_target=$dist/${dist_target}.js

  if [ -f "$src_target" ]; then
    getto_elm_build_remove_relative_module_cache $target

    echo "elm build: $target"
    "$elm" make "$src_target" --output="$tmp_target" && getto_elm_build_after
  fi
}

getto_elm_build_after(){
  case "$target" in
    */App/*/Page.elm)
      if [ -n "$(getto_elm_build_changed)" ]; then
        mkdir -p $(dirname $dist_target)
        mv "$tmp_target" "$dist_target"
      fi
      ;;
    *.elm)
      if [ -f "$test_target" ]; then
        "$elm_test" --compiler "$elm" "$test_target"
      fi
      ;;
  esac
}
getto_elm_build_changed(){
  local check
  local changed

  if [ ! -f "$dist_target" ]; then
    changed=true
  else
    check=$(sha1sum $tmp_target)
    check=${check/$tmp_target/$dist_target}
    if [ -n "$(echo "$check" | sha1sum -c --quiet 2> /dev/null)" ]; then
      changed=true
    fi
  fi

  if [ -n "$changed" ]; then
    echo changed
  fi
}

getto_elm_build_remove_relative_module_cache(){
  local current
  local module
  local file

  current=$1; shift

  module=${current%.elm}
  module=${module//\//.}

  rm -f $elm_cache/${module//./-}.*

  for file in $(grep "import $module " -R $target_dir | sed 's/:.*//'); do
    file=${file#$target_dir}
    file=${file#/}

    getto_elm_build_remove_relative_module_cache $file
  done
}

getto_elm_build_main "$@"
