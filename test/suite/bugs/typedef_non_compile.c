typedef int x; // x is now a typedef-ident
int y;         // y is now an ident
int x;         /* Should be hard error: redeclared as different kind of symbol  */
typedef int y; /* Should be hard error: redeclared as different kind of symbol  */
