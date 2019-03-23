#!/bin/bash

check_unused_import_main(){
  local file
  local module
  file=$1

  for module in $(check_unused_import_named_imports); do
    check_unused_import_check
  done
  for module in $(check_unused_import_direct_imports); do
    check_unused_import_check
  done
}
check_unused_import_check(){
  if [ -z "$(grep "$module\\." $file)" ]; then
    echo "$file:$module"
  fi
}

check_unused_import_named_imports(){
  grep "^import .* as .*" $file | grep -v " exposing " | sed 's/.* as \([^ ]*\).*/\1/'
}
check_unused_import_direct_imports(){
  grep "^import [^ ]*$" $file | sed 's/^import \([^ ]*\)/\1/'
}

check_unused_import_main "$@"
