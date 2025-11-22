#!/usr/bin/env zsh

die() {
  >&2 print -P "%F{red}%Berror%b: $1%f"
  exit 1
}

while [[ "$#" > 0 ]]
do case "$1" in
  -v | --verbose) VERBOSE=1
  ;;
  *) if [[ -n $SOURCE ]]; then
    die "unexpected extra argument '$1'"
  else
    SOURCE="$1"
  fi
esac
shift
done

if [[ -z $SOURCE ]]; then
  die 'missing required argument <SOURCE>'
fi

print -P "%S-- RUSTC --------------------------------%s"
printf -- "$SOURCE" | rustc +nightly - -Zparse-crate-root-only $([[ -z $VERBOSE ]] && echo "--error-format=short")
RUSTC_RESULT="$?"

print -P "%S-- RASUR --------------------------------%s"
./rasur --source "$SOURCE" $([[ -z $VERBOSE ]] && echo "--short")
RASUR_RESULT="$?"

[[ $RUSTC_RESULT == $RASUR_RESULT ]]
