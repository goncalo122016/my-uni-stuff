#define VALUE 25
#include <stdio.h>

int a, res;
int main () {
  int b;
  a = VALUE;
  scanf("%d", &b);
  res = a+b;
  printf("%d + %d = %d\n", a, b, res);
}