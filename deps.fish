#!/usr/bin/env fish

set dot_file (mktemp)

echo "digraph G {" > $dot_file
echo " node [shape=component, color=\"#cccccc\", fontname=monospace];" >> $dot_file

set int_modules

for f in *.el
    set mod (string match -r "\(provide \'(.+)\)" (cat $f))[2]
    echo " \"$mod\" [color=\"#333333\"];" >> $dot_file
    set int_modules $int_modules $mod
end

for f in *.el
    set mod (string match -r "\(provide \'(.+)\)" (cat $f))[2]
    if test -n $mod
        for dep in (string match -r "\(require \'(.+)\)" (cat $f))
            if string match -r "\(req" $dep > /dev/null
            else
                if contains $dep $int_modules
                    echo "\"$mod\" -> \"$dep\" [color=\"#333333\"];" >> $dot_file
                else
                    echo "\"$mod\" -> \"$dep\" [color=\"#cccccc\"];" >> $dot_file
                end
            end
        end
    end
end

echo "}" >> $dot_file

set img_file (mktemp)
dot -Tpng $dot_file -o $img_file && rm $dot_file
feh $img_file && rm $img_file
