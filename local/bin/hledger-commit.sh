#!/usr/bin/env bash

cd ~/miscellany
git commit -am "Scheduled daily journal commit"
sleep 7m && git push origin master && \
focus "Pushed to miscellany on github." 2 20 &
