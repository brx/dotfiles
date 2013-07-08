#!/bin/sh

BOOTSTRAP_DIR=`dirname $0`
cd $BOOTSTRAP_DIR/..

for f in `find . -maxdepth 1 -not -regex "\.\(/\(bin\|\.git\|\.gitignore\)\(/.*\)?\)?"`; do
    fname=`basename $f`
    src=`pwd`/$fname
    tgt=~/$fname
    if [[ -e $tgt && ! -h $tgt ]]; then
        echo $tgt exists and is not a symlink, please delete manually
        echo before invoking $0 again.
        exit 1
    else
        echo $tgt "~>"  $src
        ln -snf $src $tgt
    fi
done
