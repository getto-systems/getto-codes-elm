#!/bin/bash

for page in $(find -L src -name Page.elm); do
  if [ -n "$(echo $page | grep App)" ]; then
    ./bin/build.sh "./src" "${page#src/}"
  fi
done
