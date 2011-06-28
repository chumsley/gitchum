#!/bin/sh

# Move to the toplevel repo directory
while [ ! -d .git -a `pwd` != / ]; do cd ..; done
if [ `pwd` = / ]; then
    echo cvs_update.sh: Error, cannot find toplevel git repo directory!
    exit 1
fi

# Don't try to do this with dirty state
CURRENT_STASHES=`git stash list`
git stash
NEW_STASHES=`git stash list`
if [ "no$NEW_STASHES" != "no$CURRENT_STASHES" ]; then
    echo Cannot update with unrecorded changes.  It just isn\'t safe.
    git stash pop
    exit 1
fi

# Ensure that we are up to date
cvs update
