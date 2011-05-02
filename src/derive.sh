#!/bin/bash
set -o errexit
DERIVE=./derive/Derive
DERIVE_PATCH_VERSION=2.4.2
if ghc-pkg find-module Data.DeriveMain | grep -q '^[ ]*derive-'; then
    (cd derive && ghc -O --make -o Derive Derive.hs)
fi
if [ ! -e ${DERIVE} ] ; then
	echo "Warning: Could not find ${DERIVE}, and derive >= 2.5 is not installed">&2
        echo "Press Enter to download, patch and build derive-${DERIVE_PATCH_VERSION}.">&2
	read
	cabal unpack derive-${DERIVE_PATCH_VERSION}
	pushd derive-${DERIVE_PATCH_VERSION}
	patch -p1 < ../derive-${DERIVE_PATCH_VERSION}.patch
	cabal configure
	cabal build
	popd
	echo "Installing ${DERIVE}"
	cp derive-${DERIVE_PATCH_VERSION}/dist/build/derive/derive "${DERIVE}"
fi
TARGETS="Language/C/Syntax/AST.hs Language/C/Analysis/SemRep.hs"
for T in ${TARGETS} ; do
	echo "Appending derived instances to ${T}"
	$DERIVE -a "${T}"
done
