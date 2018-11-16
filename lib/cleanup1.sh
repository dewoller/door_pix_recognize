#!/bin/sh
textevent=$1
category=`curl -s "http://127.0.0.1:8000/predict?text_event=$textevent"`
filename=`curl -s "http://127.0.0.1:8000/bestfile?text_event=$textevent"`

echo cp $filename /mnt/video/check/$category
cp $filename /mnt/video/check/$category
