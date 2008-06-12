void foo() __attribute__((noreturn, noreturn))
           __attribute__((noreturn));
/* From the gnu examples */
__attribute__((noreturn)) void 
  d0 (void),
  __attribute__((format(printf, 1, 2))) d1 (const char *, ...),
  d2 (void) ;