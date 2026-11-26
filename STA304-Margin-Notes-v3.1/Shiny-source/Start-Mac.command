#!/bin/zsh
# Launch from Finder, Terminal or a directory whose name contains spaces.
emulate -LR zsh
cd -q -- "${0:A:h}" || exit 1

if [[ ! -r app/app.R ]]; then
  print -u2 'Extract the entire ZIP first, then double-click Start-Mac.command.'
  exit 1
fi

typeset -aU candidates
candidates=("${commands[Rscript]:-}" /Library/Frameworks/R.framework/Resources/bin/Rscript /opt/homebrew/bin/Rscript /usr/local/bin/Rscript)
for engine in "${candidates[@]}"; do
  [[ -x "$engine" && -f "$engine" ]] || continue
  if "$engine" --vanilla tools/launch.R check >/dev/null 2>&1; then
    print 'Opening the local Shiny demo. Keep this Terminal window open while using it.'
    trap 'exit 0' INT
    if "$engine" --vanilla tools/launch.R run; then exit 0; fi
    break
  fi
done

print 'Opening the offline interactive demo in your browser.'
if ! open "$PWD/../OPEN WEBSITE.html"; then
  print -u2 'Double-click OPEN WEBSITE.html in the parent folder to open the demo.'
  exit 1
fi
