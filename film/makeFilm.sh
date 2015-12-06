#!/bin/bash

for i in *.ppm; do
    convert $i ${i%.*}.png
done

rm *.ppm
ffmpeg -start_number 1 -i film%d.png -c:v libx264 -preset ultrafast -qp 0 -c:a copy film.avi
