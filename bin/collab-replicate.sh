#!/usr/bin/env bash
# usage: ./collab-replicate.sh <YYYY-MM[-DD]> <repo-url>
#
# Clone <repo-url> to collab-<YYYY-MM[-DD]/meetup0. Remove the .git directory,
# and use meetup0 as the source to make copies for each of meetup1 - meetup9.
#
# Update the current-collab symlink.

set -ex

COLLAB_DIR=collab-${1}
REPO_URL=$2

mkdir -p ${COLLAB_DIR}
git clone ${REPO_URL} ${COLLAB_DIR}/meetup0
rm -rf ${COLLAB_DIR}/meetup0/.git

for group in {1..9}; do
    rsync -av ${COLLAB_DIR}/meetup0/ ${COLLAB_DIR}/meetup${group}/
done

rm current-collab && ln -s ${COLLAB_DIR} current-collab
