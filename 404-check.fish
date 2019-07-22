#!/usr/bin/env fish

set links
for f in **.html
    for link in (cat $f | pup "a attr{href}" | grep "^http")
        set -a links $link
    end
end

set links (printf '%s\n' $links | sort -u)

function link_alive_p
    if test (curl -s -o /dev/null -w "%{http_code}" $argv[1]) = "404"
        echo "[x] $argv[1]" 1>&2
    end
end

echo "Total "(count $links)" unique external links found"
echo "Errors will go in stderr"
echo ""

set i 1
for link in $links
    echo "$i :: $link"
    link_alive_p $link
    set i (math $i + 1)
end
