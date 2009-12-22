#!/bin/sh

TITLE="`mocp -i | grep 'Title:' | sed -e 's/^.*: //'`";
if [ "$TITLE" != "" ]; then
ARTIST="`mocp -i | grep 'Artist:' | sed -e 's/^.*: //'`";
SONGTITLE="`mocp -i | grep 'SongTitle:' | sed -e 's/^.*: //'`";
if [ "$ARTIST" != "" ]; then ARTIST="$ARTIST - "; fi
echo $ARTIST $SONGTITLE
else echo Stopped
fi
