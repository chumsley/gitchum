#!/bin/sh

if [ no$* = no -o $* = --help ]; then
    echo Usage: cvs_checkout.sh \<module_name\>
    exit 1
fi

# get the files
cvs checkout $1 || exit 1
cd $1

# Add files to master branch
git init
if [ no`find . -name CVS -exec cvs_entries.sh '{}' \;` = no ]; then
    ADDED_FILES=false
else
    ADDED_FILES=true
fi    
find . -name CVS -exec cvs_entries.sh '{}' \; | while read line; do
    git add "$line" || exit 1
done
if $ADDED_FILES; then
    git commit -m "Initial commit from CVS module: $1" || exit 1
else
    touch dummy
    git add dummy
    git commit -m "Initial commit of dummy file" || exit 1
    git rm dummy
    git commit -m "Removed dummy file" || exit 1
fi

