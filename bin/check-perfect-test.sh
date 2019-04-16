#!/bin/bash

check_perfect_test(){
  local file

  for file in $(find -L src -type f -name *.elm); do
    case $file in
      */View/*)
        file=tests/${file#src/}
        file=${file%.elm}Test.elm
        if [ ! -f $file ]; then
          echo "TEST REQUIRED: $file"
          exit 1
        fi
        ;;
    esac
  done

  echo "perfect test: OK"
}

check_perfect_test "$@"
