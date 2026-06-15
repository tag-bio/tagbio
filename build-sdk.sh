#!/usr/bin/env bash
#
# build-sdk.sh — rebuild the tagbio R SDK tarball with one command.
#
# Automates the README release runbook:
#   1. bump tagbio/DESCRIPTION Version (auto-increment the patch, or use the
#      version you pass)
#   2. R CMD build tagbio      -> tagbio_<version>.tar.gz at the repo root
#   3. repoint tagbio_latest.tgz -> the new tarball
#   4. (optional) git tag v<version>
#
# Usage:
#   ./build-sdk.sh                 # auto-bump patch (e.g. 1.1.63 -> 1.1.64)
#   ./build-sdk.sh 1.2.0           # set an explicit version
#   ./build-sdk.sh --tag           # auto-bump AND create git tag v<version>
#   ./build-sdk.sh 1.2.0 --tag     # explicit version AND tag
#   ./build-sdk.sh -h              # this help
#
# After this, install it on the target R env:  R CMD INSTALL tagbio_latest.tgz
# -----------------------------------------------------------------------------
set -euo pipefail

# always operate from the repo root (this script's own directory)
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT"

DESC="tagbio/DESCRIPTION"
[[ -f "$DESC" ]] || { echo "ERROR: $DESC not found - run from inside the tagbio repo." >&2; exit 1; }
command -v R >/dev/null 2>&1 || { echo "ERROR: R is not on PATH." >&2; exit 1; }

# ---- parse args: optional explicit X.Y.Z version and/or --tag ----------------
NEWVER=""; DO_TAG=0
for a in "$@"; do
  case "$a" in
    --tag)     DO_TAG=1 ;;
    -h|--help) sed -n '2,28p' "$0"; exit 0 ;;
    *)
      if [[ "$a" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        NEWVER="$a"
      else
        echo "ERROR: unrecognized arg '$a' (expected X.Y.Z or --tag). Try -h." >&2; exit 1
      fi ;;
  esac
done

CURVER="$(grep -E '^Version:' "$DESC" | awk '{print $2}')"
if [[ -z "$NEWVER" ]]; then
  base="${CURVER%.*}"; patch="${CURVER##*.}"
  [[ "$patch" =~ ^[0-9]+$ ]] || { echo "ERROR: cannot auto-bump non-numeric patch in '$CURVER'; pass a version." >&2; exit 1; }
  NEWVER="${base}.$((patch + 1))"
fi
echo "==> Version: ${CURVER} -> ${NEWVER}"

# ---- 1. bump DESCRIPTION (portable in-place edit) ---------------------------
sed -i.bak -E "s/^Version: .*/Version: ${NEWVER}/" "$DESC" && rm -f "${DESC}.bak"

# ---- 2. build ---------------------------------------------------------------
echo "==> R CMD build tagbio"
R CMD build tagbio

TARBALL="tagbio_${NEWVER}.tar.gz"
[[ -f "$TARBALL" ]] || { echo "ERROR: R CMD build did not produce ${TARBALL}." >&2; exit 1; }

# ---- 3. repoint the symlink -------------------------------------------------
ln -sfn "$TARBALL" tagbio_latest.tgz
echo "==> tagbio_latest.tgz -> ${TARBALL}"

# ---- verify the built tarball actually carries the new version --------------
INVER="$(tar -xzf "$TARBALL" -O tagbio/DESCRIPTION 2>/dev/null | grep -E '^Version:' | awk '{print $2}')"
[[ "$INVER" == "$NEWVER" ]] || echo "WARN: tarball DESCRIPTION version ('${INVER}') != ${NEWVER}." >&2

# ---- 4. optional git tag ----------------------------------------------------
if [[ "$DO_TAG" -eq 1 ]]; then
  if git rev-parse --git-dir >/dev/null 2>&1; then
    git tag "v${NEWVER}" && echo "==> git tag v${NEWVER}"
  else
    echo "WARN: not a git repo - skipped tag." >&2
  fi
fi

echo
echo "Done: built ${TARBALL} (verified ${INVER}); tagbio_latest.tgz updated."
echo "Next: install on the target R env ->  R CMD INSTALL tagbio_latest.tgz"
[[ "$DO_TAG" -eq 1 ]] || echo "      (release tag, when ready: git tag v${NEWVER})"
