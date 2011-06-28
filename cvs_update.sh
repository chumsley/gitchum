#!/bin/sh

# Move to the toplevel repo directory
while [ ! -d .git -a `pwd` != / ]; do cd ..; done
if [ `pwd` = / ]; then
    echo cvs_commit.sh: Error, cannot find toplevel git repo directory!
    exit 1
fi

# Ensure that CVS branch is up to date
# (Ensures that ff will fail on conflicts)
cvs_fetch.sh || exit 1

# Merge in the changes from cvs
git merge cvs_head
