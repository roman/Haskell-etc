#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

pushd "$DIR/.." > /dev/null
CONFIG=$(base64 -w 0 resources/spec.yaml | tr '\n' "\\n")
sed -e "s/_data_/${CONFIG}/g" src/Config.tpl.hs > src/Config.hs
popd > /dev/null
