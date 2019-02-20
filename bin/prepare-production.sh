#!/bin/bash

prepare_production_replace(){
  sed \
    -e 's|$KEYCLOAK_URL|'$KEYCLOAK_URL'|g' \
    -e 's|$API_URL|http://'$LABO_IP':'$LABO_PORT_PREFIX'81|g' \
    -e 's|$RELOAD_URL|ws://'$LABO_IP':'$LABO_PORT_PREFIX'80|g' \
    $1 > $2
}

prepare_production_replace config/templates/env/production.elm modules/Env.elm

mkdir -p tmp/templates

prepare_production_replace config/templates/html.ejs   tmp/templates/html.ejs
prepare_production_replace config/templates/config.ejs tmp/templates/config.ejs
