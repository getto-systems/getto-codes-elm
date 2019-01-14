#!/bin/bash

getto_elm_test_main(){
  local elm
  local elm_test
  local target_dir
  local target
  local test_target

  elm=./node_modules/.bin/elm
  elm_test=./node_modules/.bin/elm-test

  if [ $# -eq 0 ]; then
    "$elm_test" --compiler "$elm"
  else
    target_dir=$1; shift
    target=$1; shift

    test_target=tests/$target

    if [ -f "$test_target" ]; then
      echo "elm test: $target"
      "$elm_test" --compiler "$elm" "$test_target"
    fi
  fi
}

getto_elm_test_main "$@"
