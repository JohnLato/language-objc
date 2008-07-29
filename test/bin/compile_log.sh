gcc -fsyntax-only $1.c  2>&1 1>/dev/null | ruby -pe '$_ = $_.gsub(/[\w_-]+\.c:[\d:]*/,"@POS")' > $1.log
$BINDIR/CTest $1.c | gcc -x c -fsyntax-only - 2>&1 1>/dev/null | ruby -pe '$_ = $_.gsub(/<stdin>:[\d:]*/,"@POS")' > $1_test.log
diff -u $1.log $1_test.log