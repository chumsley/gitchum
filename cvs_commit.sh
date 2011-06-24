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

# Push committed changes to the CVS branch
echo cvs_commit.sh: Merging $CURRENT_BRANCH '-->' cvs_head
git checkout cvs_head
git merge $CURRENT_BRANCH

# Update files list
git ls-files | sort > /tmp/git_files
find . -name CVS -exec cvs_entries.sh '{}' \; | sed 's/^\.\///g' | sort > /tmp/cvs_files

NEW_FILES=`diff --context=0 /tmp/cvs_files /tmp/git_files | grep '^\+ ' | sed 's/^\+ //g'`
if [ no$NEW_FILES != no ]; then
    cvs add $NEW_FILES || exit 1
fi

DEL_FILES=`diff --context=0 /tmp/git_files /tmp/cvs_files | grep '^\+ ' | sed 's/^\+ //g'`
if [ no$DEL_FILES != no ]; then
    cvs remove $DEL_FILES || exit 1
fi

# Commit to CVS
cvs commit $*

# Restore working directory
git checkout $CURRENT_BRANCH
if [ "no$NEW_STASHES" != "no$CURRENT_STASHES" ]; then
    git stash pop
fi
