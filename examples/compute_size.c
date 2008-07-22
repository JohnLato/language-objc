#include <stdio.h>
struct t { char x; short y; } __attribute__((packed));
typedef struct t T;
union u { T x; T* y; };
struct s { 
  struct k { short b1 : 8, b2: 9, b3: 8, b4 : 7;} x; 
  union u a,*b;
};
int main() 
{
  printf("%d\n",sizeof(struct k));
  printf("%d\n",sizeof(struct s));
  printf("%d\n",sizeof(struct t));
  printf("%d\n",sizeof(union u));
}