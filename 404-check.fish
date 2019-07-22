#!/usr/bin/env fish

set links
for f in **.html
    for link in (cat $f | pup "a attr{href}" | grep "^http")
        set -a links $link
    end
end

set links (printf '%s\n' $links | sort -u)

function check_link
    switch (curl -s -o /dev/null -w "%{http_code}" $argv[1] --max-time 30)
        case 404
            echo $argv[1] 1>&2
        case 100
            echo $argv[1] 1>&2
    end
end

echo "Total "(count $links)" unique external links found"
echo "Errors will go in stderr"
echo ""

set i 1
for link in $links
    echo "$i :: $link"
    check_link $link
    set i (math $i + 1)
end
