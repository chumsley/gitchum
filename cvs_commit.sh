#!/bin/sh

# Move to the toplevel repo directory
while [ ! -d .git -a `pwd` != / ]; do cd ..; done
if [ `pwd` = / ]; then
    echo cvs_commit.sh: Error, cannot find toplevel git repo directory!
    exit 1
fi

# Save current dirty state
CURRENT_STASHES=`git stash list`
git stash
NEW_STASHES=`git stash list`

NEEDS_UPDATE=`cvs -n update | grep -v '^\?'`
if [ no$NEEDS_UPDATE != no -a "no$NEW_STASHES" != "no$CURRENT_STASHES" ]; then
    echo cvs_commit.sh: Don\'t try to commit an out-of-date sandbox that has unrecorded changes, please.
    git stash pop
    exit 1
fi

# Ensure that we are up to date and commit 
cvs update && cvs commit $*

# Restore working directory
if [ "no$NEW_STASHES" != "no$CURRENT_STASHES" ]; then
    git stash pop
fi
