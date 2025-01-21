#include <stdio.h>

int fizz(int n) {
  return ((n + 4) % 43) == 0;
}

int buzz(int n) {
  return ((n - 5) % 47) == 0;
}

void fizzbuzz(int n) {
int  *i = 0;
  if (fizz(n) && buzz(n)) {
    printf("FizzBuzz - ");
    printf ("%d\n", *i);
  }
      
  else if (fizz(n)) {
      printf("Fizz\n");
      printf ("%d\n", *i);
  }
  else if (buzz(n)) {
      printf("Buzz\n");
      printf ("%d\n", *i);
  }
  else *i++;
}

void fizzbuzz_range(int from, int to) {
    for (int i = from;  i < to; ++i) {
        fizzbuzz(i);
    }
}

int main() {

    fizzbuzz_range (467, 5467);
    return 0;
}