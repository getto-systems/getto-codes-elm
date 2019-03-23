#!/bin/bash

check_all_unused_import_main(){
  local unused
  unused=$(find modules -name "*.elm" | xargs -n 1 bin/check-unused-import.sh)
  if [ -z "$unused" ]; then
    echo "un-used import check: OK"
  else
    echo "un-used import check: NG"
    echo $unused
    exit 1
  fi
}

check_all_unused_import_main "$@"
