#!/usr/bin/env bash

cd ~/miscellany
git commit -am "Scheduled daily journal commit"

(
git push origin master
while [[ $? -ne 0 ]]; do 
    sleep 2m 
    git push origin master
done 
focus "Pushed ~/miscellany.git to github." 2 20
) &
