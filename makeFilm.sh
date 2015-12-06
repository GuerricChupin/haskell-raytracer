#!/bin/bash

for i in film/*.ppm; do
    convert $i ${i%.*}.png
done

ffmpeg -start_number 0 -i film/film%d.png -c:v libx264 -preset ultrafast -qp 0 -c:a copy film/film.avi

rm film/*.png
