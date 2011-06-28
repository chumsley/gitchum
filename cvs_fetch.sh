#!/bin/sh

# Move to the toplevel repo directory
while [ ! -d .git -a `pwd` != / ]; do cd ..; done
if [ `pwd` = / ]; then
    echo cvs_commit.sh: Error, cannot find toplevel git repo directory!
    exit 1
fi

# Save current branch name
CURRENT_BRANCH=`git branch | grep '^\*' | awk '{print $2}'`
if [ $CURRENT_BRANCH = cvs_head ]; then
    echo cvs_commit.sh: Cannot commit from cvs_head branch directly!
    exit 1
fi

# Save current dirty state
CURRENT_STASHES=`git stash list`
git stash
NEW_STASHES=`git stash list`

# Switch to the cvs_head directory and pull in any changes from the server
if git checkout cvs_head && cvs update; then
    echo cvs update ok
else
# Restore working directory
    git checkout $CURRENT_BRANCH
    if [ "no$NEW_STASHES" != "no$CURRENT_STASHES" ]; then
        git stash pop
    fi
    exit 1
fi

# Update files list
git ls-files | sort > /tmp/git_files
find . -name CVS -exec cvs_entries.sh '{}' \; | sed 's/^\.\///g' | sort > /tmp/cvs_files
diff --context=0 /tmp/git_files /tmp/cvs_files | grep '^\+ ' | sed 's/^\+ //g' > /tmp/NEW_FILES
diff --context=0 /tmp/cvs_files /tmp/git_files | grep '^\+ ' | sed 's/^\+ //g' > /tmp/DEL_FILES

cat /tmp/NEW_FILES | while read line; do
    git add "$line"
done
cat /tmp/DEL_FILES | while read line; do
    git rm "$line"
done

# Commit to git (TODO read commit comments from CVS automagically?)
git commit -m "Updated from CVS"

# Restore working directory
git checkout $CURRENT_BRANCH
if [ "no$NEW_STASHES" != "no$CURRENT_STASHES" ]; then
    git stash pop
fi
exit 1
