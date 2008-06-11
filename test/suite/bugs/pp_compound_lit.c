/* Test compound literals and statement expressions */
typedef struct point { int x; int y; } Point;
void drawline(Point x, Point y);
void drawline_(Point *x, Point *y);

/* Compound literals */
int *p = (int []) {2, 4}; 
const float* pows = (const float []) {1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6};
int examples() {
  int *q = (int [2]) { p[1], p[0] };
  drawline( (struct point){.x=1, .y=1}, (struct point){ .x = 3, .y = 4}); 
  drawline_( &(struct point){.x=1, .y=1}, &(struct point){.x=1,.y=4}); 
  
}
/* Statement expressions */
void gnu() {
  int a = 2, b = 3;
  int max_a_b = 
    ({int _a = (a), _b = (b); _a > _b ? _a : _b; });
  int complex_a_b = ({int _c = (a);
                      while (_c > 0) { _c --; a = a + b; }
                      a + b + _c; });
}
void strange() {
  char x = ( char ) { 2, } ; /* ok */
  char* y = ( char[3] ) { 'a', x, x = 'b' };
  char z =  ({ 'a', x,  x = 'b'; });
}