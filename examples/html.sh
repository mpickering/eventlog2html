#!/bin/bash
cat <<END
<!DOCTYPE html>
<html>
<head>
<title>hp2pretty</title>
</head>
<body style="width:1280px;margin:auto;">
<h1>hp2pretty</h1>
END
cat $1 |
while read svg rest
do
  re='^(.+) [(](.+)[)]$'
  if [[ $rest =~ $re ]]
  then
    title="${BASH_REMATCH[1]}"
    date="${BASH_REMATCH[2]}"
    printf '<h2><code>%s</code> (%s)</h2>\n' "${title}" "${date}"
    printf '<img src="%s" />\n' "${svg}"
  fi
done
printf '<h2>Legend</h2>\n' "${title}" "${date}"
printf '<table>\n'
cat $2 | sed 's|\\|\\\\|g' |
while read svg label
do
  printf '<tr><td><img src="%s" /></td><td><code>%s</code></td></tr>\n' "${svg}" "${label}"
done
printf '</table>\n'
cat <<END
</body>
</html>
END
