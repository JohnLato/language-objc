typedef float v4 __attribute__((vector_size(sizeof(float)*4)));
typedef struct point { int x; int y; } Point;
extern char compile_time_assert[__alignof__(v4) == sizeof(float)*4 ? 1 : -1];
extern char compile_time_assert[__alignof(v4) == sizeof(float)*4 ? 1 : -1];
extern char compile_time_assert[__alignof(v4) == sizeof(float)*4 ? 1 : -1];
