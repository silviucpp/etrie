#!/usr/bin/env bash

ROOT=$(pwd)
DEPS_LOCATION=_build/deps
OS=$(uname -s)
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')
CPUS=`getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu`

# hat-trie

HATTRIE_DESTINATION=hat-trie
HATTRIE_REPO=https://github.com/Tessil/hat-trie.git
HATTRIE_BRANCH=master
HATTRIE_TAG=906e6ab
HATTRIE_SUCCESS=include/tsl/htrie_map.h

fail_check()
{
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
        exit 1
    fi
}

CheckoutLib()
{
    if [ -f "$DEPS_LOCATION/$4/$5" ]; then
        echo "$4 fork already exist. delete $DEPS_LOCATION/$4 for a fresh checkout ..."
    else
        #repo rev branch destination

        echo "repo=$1 tag=$2 branch=$3"

        mkdir -p $DEPS_LOCATION
        pushd $DEPS_LOCATION

        if [ ! -d "$4" ]; then
            fail_check git clone -b $3 $1 $4
        fi

        pushd $4
        fail_check git checkout $2
        popd
        popd
    fi
}

CheckoutLib $HATTRIE_REPO $HATTRIE_TAG $HATTRIE_BRANCH $HATTRIE_DESTINATION $HATTRIE_SUCCESS
