#!/bin/sh
echo "deriving for Constants"
derive -u NodeDerive -a Language/C/Syntax/Constants.hs
echo "deriving for AST"
derive -u NodeDerive -a Language/C/Syntax/AST.hs
echo "deriving for SemRep"
derive -u NodeDerive -a Language/C/Analysis/SemRep.hs
