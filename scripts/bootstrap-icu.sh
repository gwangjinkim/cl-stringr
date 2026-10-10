#!/bin/sh
# Build the standalone ICU engine matching parity/conformance's stringi.
# All work stays in the foreground; no R/stringi library is loaded at runtime.
set -eu
ICU_HERE=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
ICU_PREFIX=${1:-"$ICU_HERE/.native/icu-71.1"}
ICU_JOBS=${CL_STRINGR_ICU_JOBS:-4}
ICU_URL=https://github.com/unicode-org/icu/releases/download/release-71-1/icu4c-71_1-src.tgz
ICU_SHA256=67a7e6e51f61faf1306b6935333e13b2c48abd8da6d2f46ce6adca24b1e21ebf
ICU_BUILD=$(mktemp -d "${TMPDIR:-/tmp}/cl-stringr-icu71.XXXXXX")
trap 'rm -rf "$ICU_BUILD"' EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM
ICU_ARCHIVE="$ICU_BUILD/icu4c-71_1-src.tgz"
if [ -n "${CL_STRINGR_ICU_TARBALL:-}" ]; then
  cp "$CL_STRINGR_ICU_TARBALL" "$ICU_ARCHIVE"
else
  curl -fL --retry 2 -o "$ICU_ARCHIVE" "$ICU_URL"
fi
if command -v sha256sum >/dev/null 2>&1; then
  ICU_ACTUAL=$(sha256sum "$ICU_ARCHIVE" | cut -d ' ' -f 1)
else
  ICU_ACTUAL=$(shasum -a 256 "$ICU_ARCHIVE" | cut -d ' ' -f 1)
fi
[ "$ICU_ACTUAL" = "$ICU_SHA256" ] || { echo "ICU source checksum mismatch" >&2; exit 1; }
tar -xzf "$ICU_ARCHIVE" -C "$ICU_BUILD"
cd "$ICU_BUILD/icu/source"
# Host environment flags can accidentally select an incompatible installed ICU.
ICU_LDFLAGS=
if [ "$(uname -s)" = Darwin ]; then ICU_LDFLAGS=-Wl,-headerpad_max_install_names; fi
env CPPFLAGS= CFLAGS= CXXFLAGS= LDFLAGS="$ICU_LDFLAGS" ./configure --prefix="$ICU_PREFIX" \
  --disable-tests --disable-samples --disable-static --enable-shared
if command -v gmake >/dev/null 2>&1; then ICU_MAKE=gmake; else ICU_MAKE=make; fi
"$ICU_MAKE" -j "$ICU_JOBS"
"$ICU_MAKE" install
# ICU71's Darwin install names are relative; use relocatable sibling dylibs.
if [ "$(uname -s)" = Darwin ]; then
  for file in "$ICU_PREFIX"/lib/*.71.1.dylib; do
    base=$(basename "$file")
    install_name_tool -id "@rpath/$base" "$file"
    for dep in libicudata.71.dylib libicuuc.71.dylib libicui18n.71.dylib libicuio.71.dylib libicutu.71.dylib libicutest.71.dylib; do
      install_name_tool -change "$dep" "@loader_path/$dep" "$file"
    done
    codesign --force --sign - "$file"
  done
fi
printf '%s\n' "$ICU_SHA256" > "$ICU_PREFIX/.source-sha256"
printf 'ICU 71.1 installed in %s\n' "$ICU_PREFIX"
