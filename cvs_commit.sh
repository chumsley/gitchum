#!/bin/sh

# Move to the toplevel repo directory
while [ ! -d .git -a `pwd` != / ]; do cd ..; done
if [ `pwd` = / ]; then
    echo cvs_commit.sh: Error, cannot find toplevel git repo directory!
    exit 1
fi

# Don't try to commit from an out-of-date sandbox
NEEDS_UPDATE=`cvs -n update | grep -v '^[?RAM]'`
if [ "no$NEEDS_UPDATE" != "no" ]; then
    echo Sandbox is out-of-date\; update needed:
    cvs -n update
    exit 1
fi

# Save current dirty state
CURRENT_STASHES=`git stash list`
git stash
NEW_STASHES=`git stash list`

# Update files list
# We add files in git to CVS but not vice versa, because our personal
# use-case involves uploading PDF files into CVS that we don't want to track
# in git.  (So we don't want CVS to interpret that as the PDFs having been
# deleted from git).  We might occasionally have to manually remove a file
# from CVS, but that's not the end of the world.
git ls-files | sort > /tmp/git_files
find . -name CVS -exec cvs_entries.sh '{}' \; | sed 's/^\.\///g' | sort > /tmp/cvs_files
diff --context=0 /tmp/cvs_files /tmp/git_files | grep '^\+ ' | sed 's/^\+ //g' > /tmp/NEW_FILES
cat /tmp/NEW_FILES | while read line; do
    cvs add "$line"
done

# Generate log file
git log last_cvs_commit.. | grep '^    \|^$\|^Date:' > /tmp/CVS_LOG_MESSAGE

# send the changes and adjust the last-commit tag
cvs commit -F /tmp/CVS_LOG_MESSAGE $* && git tag -f last_cvs_commit

# Restore working directory
if [ "no$NEW_STASHES" != "no$CURRENT_STASHES" ]; then
    git stash pop
fi
