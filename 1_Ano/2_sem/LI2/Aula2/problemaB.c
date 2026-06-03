#include <stdio.h>
#include <locale.h>
#include <wchar.h>

int main()
{
    /* Mudar para UTF8 */
    setlocale(LC_CTYPE, "C.UTF-8");

    int tabela [4] [16] = {{0x00004DC0, 0x00004DC1, 0x00004DC2, 0x00004DC3, 0x00004DC4, 0x00004DC5, 0x00004DC6, 0x00004DC7, 0x00004DC8, 0x00004DC9, 0x00004DCA, 0x00004DCB, 0x00004DCC, 0x00004DCD, 0x00004DCE, 0x00004DCF},
                           {0x00004DD0, 0x00004DD1, 0x00004DD2, 0x00004DD3, 0x00004DD4, 0x00004DD5, 0x00004DD6, 0x00004DD7, 0x00004DD8, 0x00004DD9, 0x00004DDA, 0x00004DDB, 0x00004DDC, 0x00004DDD, 0x00004DDE, 0x00004DDF},
                           {0x00004DE0, 0x00004DE1, 0x00004DE2, 0x00004DE3, 0x00004DE4, 0x00004DE5, 0x00004DE6, 0x00004DE7, 0x00004DE8, 0x00004DE9, 0x00004DEA, 0x00004DEB, 0x00004DEC, 0x00004DED, 0x00004DEE, 0x00004DEF},
                           {0x00004DF0, 0x00004DF1, 0x00004DF2, 0x00004DF3, 0x00004DF4, 0x00004DF5, 0x00004DF6, 0x00004DF7, 0x00004DF8, 0x00004DF9, 0x00004DFA, 0x00004DFB, 0x00004DFC, 0x00004DFD, 0x00004DFE, 0x00004DFF}
                           };
    unsigned long long int n;
    wchar_t caracteres[100];  
    int i = 0;

    if (scanf ("%llu", &n) == 1) {
        if (n == 0) {
                wchar_t a = 0x00004DC0;
                wprintf(L"%lc", a);
        } 
        else if (n > 0) { 

            while (n > 0) {
                int r = n % 64;
                int linha = r / 16;
                int coluna = r % 16;
                wchar_t c = tabela [linha][coluna];
                caracteres[i++] = c;
                n /= 64;
            }

        // Imprimir na ordem inversa
        for (int j = i - 1; j >= 0; j--) {
            if (j>0) wprintf(L"%lc ", caracteres[j]);  // Quando são dois ou mais caracteres (espaços entre digitos)
            else wprintf(L"%lc", caracteres[j]);  // Quando é apenas um caracter ou o último digito de um número (sem espaço no final)
        }
    } 
}
    wprintf(L"\n");
    return 0;
}