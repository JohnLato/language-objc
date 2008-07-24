/* Yes ! The first bug I found in CIL :) */
struct s { int x; } __attribute__((packed)) 
const __attribute((deprecated)) S_CONST = { 3 };
int main() { return S_CONST.x; }
/* Expected -Wall warnings: S_CONST is deprecated (s) */
