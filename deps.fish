#!/usr/bin/env fish

set dot_file (mktemp)
set img_file (mktemp)

function cleanup --on-process-exit $fish_pid
    rm $dot_file $img_file
end

echo "digraph G {" > $dot_file
echo " node [shape=component, color=\"#cccccc\", fontname=monospace, fontcolor=\"#cccccc\"];" >> $dot_file

set int_modules

for f in **.el
    set mod (cat $f | string match -r "\(provide \'(.+)\)")[2]
    if test -n "$mod"
        echo " \"$mod\" [color=\"#333333\", fontcolor=\"#333333\"];" >> $dot_file
        set -a int_modules $mod
    end
end

for f in **.el
    set mod (cat $f | string match -r "\(provide \'(.+)\)")[2]
    if test -n "$mod"
        for dep in (cat $f | string match -r "\(require \'(.+)\)")
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

dot -Tpng $dot_file -o $img_file && feh $img_file
