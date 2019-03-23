#!/bin/bash

[ -z "$(find modules -name "*.elm" | xargs -n 1 bin/check-unused-import.sh)" ]
