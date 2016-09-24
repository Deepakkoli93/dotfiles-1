#!/usr/bin/env bash

cd ~/miscellany
# Commit today's changes
git commit -am "Scheduled daily journal commit"

# Pull work subtree
git subtree pull --prefix=work/ work master

(
git push origin master
while [[ $? -ne 0 ]]; do 
    sleep 2m 
    git push origin master
done 
focus "Pushed ~/miscellany.git to github." 2 20
) &
