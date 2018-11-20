#!/bin/bash

# Environment Variable Requirements:
# - SSHKEY: A base64 encoded private SSH key that has its public key added to the repo
# - GITHUB_ACCOUNT: Account name to use when creating PR requests
# - GITHUB_TOKEN: Personal access token for the GITHUB_ACCOUNT

set -e

IFS=$'\n'

update_branch() {
    echo "Updating $1 branch..."
    if [[ "$1" == "master" ]]; then
        git checkout $1
    else
        git checkout -b $1 origin/$1
    fi

    git reset --hard $1
    git merge upstream/$1 --no-edit
    git push origin $1
}

setup_ssh() {
    echo "Adding SSH key..."
    echo $SSHKEY | base64 --decode > ~/.ssh/SSHKEY
    chmod 400 ~/.ssh/SSHKEY
    ssh-add -D
    ssh-add ~/.ssh/SSHKEY
}

setup_git() {
    echo "Setting up git config..."
    git config --global user.email "derp@opentelecom.foundation"
    git config --global user.name "OTF Git Syncaroo"

    echo "Adding upstream..."
    git remote add upstream https://github.com/2600hz/kazoo.git

    echo "Fetching from upstream..."
    git fetch upstream
}

cleanup() {
    # Doing this for debug purposes, if we need to debug the script
    # then we need to be back on the otf branch as that is the only
    # place the script exists.
    git checkout otf-master
    git reset --hard otf-master
}

generate_pr_post_body() {
    cat <<EOF
{
    "title": "Upstream merge: $1",
    "body": "Automated daily PR. Deal with this ASAP please! :D",
    "head": "$2",
    "base": "otf-$1"
}
EOF
}

pr_upstream() {
    PRS=$(curl --silent -X GET "https://api.github.com/repos/$CIRCLE_PROJECT_USERNAME/kazoo/pulls")
    COUNT=$(echo $PRS | jq '. | length')
    DATE=`date +%Y-%m-%d`
    BRANCH="upstream-sync-$1-$DATE"

    if [ "$COUNT" != "0" ]
    then
        for prtitle in $(echo "$PRS" | jq -r '.[].title'); do
            if [ "$prtitle" == "Upstream merge: $1" ]
            then
                echo "Found existing PR for this branch ($1), skipping the creation of a new one."
                return
            fi
        done
    fi

    git checkout origin/$1
    git reset --hard origin/$1
    git fetch
    git checkout -b $BRANCH
    git push -u origin $BRANCH

    curl --silent \
        -u $GITHUB_ACCOUNT:$GITHUB_TOKEN \
        -H "Content-Type: application/json" \
        -X POST "https://api.github.com/repos/$CIRCLE_PROJECT_USERNAME/kazoo/pulls" \
        --data "$(generate_pr_post_body $1 $BRANCH)"
}

# Quick test to see if we should run this script. If you're
# trying to debug/run this script in your repo add your
# username for circleci to this section.
if [[ "$CIRCLE_PROJECT_USERNAME" =~ ^(regner|OpenTelecom)$ ]]; then
    echo "$CIRCLE_PROJECT_USERNAME is in the list of repos to run against..."
else
    echo "$CIRCLE_PROJECT_USERNAME is not in the list of repos to run against. Aborting."
    exit 0
fi

setup_ssh
setup_git

update_branch $1
pr_upstream $1

cleanup
