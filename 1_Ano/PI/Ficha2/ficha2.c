#include <stdio.h>

float multInt1 (int n, float m) {
    int r = 0.0;
    if (n<0) {
        n = -n;        
        m = -m;
    }
    for (int i = 0; i<n ; i++) {
        r+=m;
    }
    return r;
}

float multInt2 (int n, float m, int *contaOps) {
    float r = 0.0;
    *contaOps = 0;

    for (n; n>=1; n=n/2) {
        if (n%2 != 0) {
            r+=m;
            (*contaOps)++;
        }
        m*=2;
        (*contaOps)++;
    }
    return r;
}

int mdc1 (int a, int b) {
    int resto;
    if (a==0) return b;
    else return b;
    do {
        resto = a%b;
        a=b;
        b=resto;
    } while (resto != 0);
    return a;
}

int mdc2 (int a, int b) {
    while (a!=0 && b!=0) {
        if (a>b) a-=b;
        else if (b>a) b-=a;
        else return a;
    }
    if (a==0) return b;
    else return a;
}

int mdc3 (int a, int b) {
    while (a!=0 && b!=0) {
        if (a>b) a%=b;
        else if (b>a) b%=a;
        else return a;
    }
    if (a==0) return b;
    else return a;
}

int fib1 (int n) {
    if (n<2) return 1;
    else return fib1(n-1) + fib1 (n-2);
}

int fib2 (int n) {
    int aux1 = 1;
    int aux2 = 1; 
    int res = 1;
    for (int i = 2; i<=n; i++) {
        res = aux2;
        aux2 += aux1;
        aux1 = res;
    }
    return aux2;
}

int main() {
    int n;
    float m;
    int contaOps;

    printf ("Digite dois números: ");
    scanf ("%d%f", &n, &m);
    printf ("Por M1: %f\n", multInt1 (n,m));

    float result = multInt2 (n,m, &contaOps);
    printf ("Por M2: %f , Operações: %d\n", result, contaOps);

    printf ("MDC1: %d\n", mdc1 (n,m));
    printf ("MDC2: %d\n", mdc2 (n,m));
    printf ("MDC3: %d\n", mdc3 (n,m));

    printf ("Fibonacci1: %d\n", fib1 (n));
    printf ("Fibonacci2: %d\n", fib2 (n));

    return 0;
}