#!/bin/bash
set -o errexit
DERIVE=../scripts/derive
DERIVE_VERSION=2.4.2
if [ ! -e ${DERIVE} ] ; then
	echo "Warning: Could not find ${DERIVE}. Press Enter to download, patch, build and install it."
	read
	cabal unpack derive-${DERIVE_VERSION}
	pushd derive-${DERIVE_VERSION}
	patch -p1 < ../derive-${DERIVE_VERSION}.patch
	cabal configure
	cabal build
	popd
	echo "Installing ${DERIVE}"
	cp derive-${DERIVE_VERSION}/dist/build/derive/derive "${DERIVE}"
fi
TARGETS="Language/C/Syntax/AST.hs Language/C/Analysis/SemRep.hs"
for T in ${TARGETS} ; do
	echo "Appending derived instances to ${T}"
	$DERIVE -a "${T}"
done
