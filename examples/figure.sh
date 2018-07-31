#!/bin/bash
cat $1 |
while read svg rest
do
  re='^(.+) [(](.+)[)]$'
  if [[ $rest =~ $re ]]
  then
    title="${BASH_REMATCH[1]}"
    date="${BASH_REMATCH[2]}"
    printf '\\begin{figure}[!ht]\n'
    printf '\\centering\n'
    printf '\\caption{\\lstinline[columns=fixed]{%s} (%s)}\n' "${title}" "${date}"
    printf '\\includesvg[width=\\textwidth,pretex=\\small]{%s}\n' "${svg}"
    printf '\\end{figure}\n'
  fi
done
printf '\\begin{table}[!ht]\n'
printf '\\centering\n'
printf '\\caption{Legend}\n'
printf '\\begin{tabular}{cl}\n'
cat $2 | sed 's|\\|\\\\|g' |
while read svg label
do
  printf '\includesvg[width=0.02109375\\textwidth]{%s} & |%s| \\\\\n' "${svg}" "${label}"
done
printf '\\end{tabular}\n'
printf '\\end{table}\n'
