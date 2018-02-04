find ./src/ -type f -name '*' -print0 | xargs -0 sed -i 's/\t/    /g'

