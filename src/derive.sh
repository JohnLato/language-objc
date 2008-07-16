#!/bin/sh
echo "deriving for Constants"
derive -INodeDerive -a Language/C/Common/Constants.hs 
echo "deriving for AST"
derive -INodeDerive -a Language/C/Parser/AST.hs 
echo "deriving for SemRep"
derive -INodeDerive -a Language/C/Analysis/SemRep.hs 
