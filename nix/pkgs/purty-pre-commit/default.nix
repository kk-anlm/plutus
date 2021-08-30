{ writeShellScriptBin, purty }:

writeShellScriptBin "purty" ''
  for f in "$@"; do
    ${purty}/bin/purty --write $f
  done
''
