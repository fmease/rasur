#!/usr/bin/env zsh

die() {
  >&2 print -P "%F{red}%Berror%b: $1%f"
  exit 1
}

while [[ "$#" > 0 ]]
do case "$1" in
  -e | --edition)
  EDITION="$2"
  [[ -z $EDITION ]] && die 'missing argument `EDITION` for option `--edition`'
  shift
  ;;
  -T | --toolchain)
  TOOLCHAIN="$2"
  [[ -z "$TOOLCHAIN" ]] && die 'missing argument `TOOLCHAIN` for option `--toolchain`'
  shift
  ;;
  -v | --verbose) VERBOSE=1
  ;;
  -f | --file) FILE=1
  ;;
  --format | --fmt) FORMAT=1
  ;;
  --ast) AST=1
  ;;
  *) if [[ -n $SOURCE ]]; then
    die "unexpected extra argument \`$1\`"
  else
    SOURCE="$1"
  fi
esac
shift
done

if [[ -n $FORMAT && -n $AST ]]; then
  die '`--format` and `--ast` are mutually exclusive'
fi

if [[ -z $SOURCE ]]; then
  die 'missing argument `SOURCE`'
fi

print

print -P '%S-- RUSTC --------------------------------%s'
printf -- "$([[ -z $FILE ]] && echo "$SOURCE")" | rustc \
  +$([[ -n "$TOOLCHAIN" ]] && echo "$TOOLCHAIN" || echo nightly) \
  $([[ -n $FILE ]] && printf -- "$SOURCE" || printf '-\n') \
  -Zparse-crate-root-only \
  $([[ -n $EDITION ]] && echo --edition "$EDITION") \
  $([[ -z $VERBOSE ]] && echo --error-format=short) \
  $([[ -n $FORMAT ]] && echo -Zunpretty=normal) \
  $([[ -n $AST ]] && echo -Zunpretty=ast-tree)
RUSTC_RESULT="$?"

print -P '%S-- RASUR --------------------------------%s'
./rasur \
  $([[ -z $FILE ]] && echo --source) "$SOURCE" \
  $([[ -n $EDITION ]] && echo --edition "$EDITION") \
  $([[ -z $VERBOSE ]] && echo --short) \
  $([[ -n $FORMAT ]] && echo --fmt) \
  $([[ -n $AST ]] && echo --ast)
RASUR_RESULT="$?"

RESULT=$(( $RUSTC_RESULT != $RASUR_RESULT ))

if [[ $RESULT != 0 ]]; then
  print -P '%S%F{red}.. MISMATCH! ............................%f%s'
else
  print -P '%S%F{green}.. MATCH! ...............................%f%s'
fi

print

exit $RESULT
