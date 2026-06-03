typedef struct histograma *Histograma;

int init(Histograma *h);

int insert (Histograma h, char *palavra);

int maisFrequente (Histograma h, char *palavra);

void liberta (Histograma h);