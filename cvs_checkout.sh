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
    echo git add \"$line\" \|\| exit 1 # TEST
    git add "$line" || exit 1
done
echo added_files $ADDED_FILES #TEST
if $ADDED_FILES; then
    git commit -m "Initial commit from $1 CVS module" || exit 1
else
    touch dummy
    git add dummy
    git commit -m "Initial commit of dummy file" || exit 1
    git rm dummy
    git commit -m "Removed dummy file" || exit 1
fi

# Create the cvs_head branch and add the CVS control files
git branch cvs_head
git checkout cvs_head
find . -name CVS -exec echo git add '{}/' \;  # TEST
find . -name CVS -exec git add '{}/' \;
git commit -m 'Added CVS control directories' || exit 1
git checkout master

