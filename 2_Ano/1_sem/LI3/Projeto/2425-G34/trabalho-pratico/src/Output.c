#include <glib.h>
#include <stdio.h>

// Função para criar e escrever o ficheiro de output
void write_output_file(const gchar *filename, const gchar *output) {
  FILE *output_file = fopen(filename, "a");

  if (output_file == NULL) {
    printf("Erro: Não foi possível abrir o arquivo %s para escrita.\n",
           filename);
  }
  if (strcmp(output, "") == 0) {
    fprintf(output_file, "%s", "\n");
  }
  fprintf(output_file, "%s", output);
  fclose(output_file);
}