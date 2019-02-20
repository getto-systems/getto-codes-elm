#!/bin/bash

prepare_development_replace(){
  sed \
    -e 's|$KEYCLOAK_URL|'$KEYCLOAK_URL'|g' \
    -e 's|$API_URL|http://'$LABO_IP':'$LABO_PORT_PREFIX'81|g' \
    -e 's|$RELOAD_URL|ws://'$LABO_IP':'$LABO_PORT_PREFIX'80|g' \
    $1 > $2
}

prepare_development_replace config/templates/headers.json headers.json

prepare_development_replace config/templates/env/development.elm modules/Env.elm

mkdir -p tmp/templates

prepare_development_replace config/templates/html.ejs   tmp/templates/html.ejs
prepare_development_replace config/templates/config.ejs tmp/templates/config.ejs
