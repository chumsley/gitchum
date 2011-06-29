#!/bin/sh

# Move to the toplevel repo directory
while [ ! -d .git -a `pwd` != / ]; do cd ..; done
if [ `pwd` = / ]; then
    echo cvs_commit.sh: Error, cannot find toplevel git repo directory!
    exit 1
fi

# Don't try to commit from an out-of-date sandbox
NEEDS_UPDATE=`cvs -n update | grep -v '^\?'`
if [ "no$NEEDS_UPDATE" != "no" ]; then
    echo Sandbox is out-of-date; update needed.
    exit 1
fi

# Save current dirty state
CURRENT_STASHES=`git stash list`
git stash
NEW_STASHES=`git stash list`

# Update files list
git ls-files | sort > /tmp/git_files
find . -name CVS -exec cvs_entries.sh '{}' \; | sed 's/^\.\///g' | sort > /tmp/cvs_files
diff --context=0 /tmp/cvs_files /tmp/git_files | grep '^\+ ' | sed 's/^\+ //g' > /tmp/NEW_FILES
diff --context=0 /tmp/git_files /tmp/cvs_files | grep '^\+ ' | sed 's/^\+ //g' > /tmp/DEL_FILES

cat /tmp/NEW_FILES | while read line; do
    cvs add "$line"
done
cat /tmp/DEL_FILES | while read line; do
    cvs remove "$line"
done

# send the changes
cvs commit $*

# Restore working directory
if [ "no$NEW_STASHES" != "no$CURRENT_STASHES" ]; then
    git stash pop
fi
