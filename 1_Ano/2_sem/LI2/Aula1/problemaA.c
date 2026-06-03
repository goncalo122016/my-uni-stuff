#include <stdio.h>
#include <locale.h>
#include <wchar.h>

// gcc -c exemplo1.c -Wall -Wextra -pedantic -O2

int main()
{
    int dia, mes;

    /* Mudar para UTF8 */
    setlocale(LC_CTYPE, "C.UTF-8");

    if (scanf("%d%d", &dia, &mes) == 2) {
        if (((dia >= 21 && mes == 3)) || ((dia <= 20 && mes == 4))) {
            wchar_t c = 0x2648;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 21 && mes == 4)) || ((dia <= 20 && mes == 5))) {
            wchar_t c = 0x2649;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 21 && mes == 5)) || ((dia <= 20 && mes == 6))) {
            wchar_t c = 0x264a;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 21 && mes == 6)) || ((dia <= 22 && mes == 7))) {
            wchar_t c = 0x264b;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 23 && mes == 7)) || ((dia <= 22 && mes == 8))) {
            wchar_t c = 0x264c;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 23 && mes == 8)) || ((dia <= 22 && mes == 9))) {
            wchar_t c = 0x264d;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 23 && mes == 9)) || ((dia <= 22 && mes == 10))) {
            wchar_t c = 0x264e;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 23 && mes == 10)) || ((dia <= 21 && mes == 11))) {
            wchar_t c = 0x264f;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 22 && mes == 11)) || ((dia <= 21 && mes == 12))) {
            wchar_t c = 0x2650;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 22 && mes == 12)) || ((dia <= 19 && mes == 1))) {
            wchar_t c = 0x2651;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 20 && mes == 1)) || ((dia <= 18 && mes == 2))) {
            wchar_t c = 0x2652;
    	    wprintf(L"%lc\n", c);
            }
        if (((dia >= 19 && mes == 2)) || ((dia <= 20 && mes == 3))) {
            wchar_t c = 0x2653;
    	    wprintf(L"%lc\n", c);
            }
    }

    return 0;
}