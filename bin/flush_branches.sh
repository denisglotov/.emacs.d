#! /bin/bash

branch=$(git symbolic-ref --short HEAD)
if [ -z "$branch" ]; then
    echo "Current branch not found. Are you in the git repo?"
    exit -1
fi
echo "Current branch is ${branch}."

merged_branches=( $(git branch --merged=${branch} | grep -v \ ${branch}) )
echo "Deleting merged branches: ${merged_branches[@]}."

git branch -d "${merged_branches[@]}"
git fetch --prune
