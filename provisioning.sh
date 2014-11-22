#!/usr/bin/env bash

apt-get update
apt-get install -y curl
printf "curl correctly installed"
apt-get install -y git
printf "git correctly installed"

apt-get install -y build-essential libncurses5-dev openssl libssl-dev fop xsltproc unixodbc-dev
printf "Erlang dependencies are correctly installed\n"

cd /usr/bin
curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl 2> /dev/null
chmod a+x kerl
printf "kerl correctly installed"

printf "** Provisioning completed **"
