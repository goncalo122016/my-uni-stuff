#include <ncurses.h>
#include <string.h>
#include <glib.h>

#include "../include/interativo/queries.h"
#include "../include/gestores/gestorUtilizador.h"
#include "../include/gestores/gestor_musica.h"
#include "../include/gestores/gestorArtista.h"
#include "../include/gestores/gestor_albums.h"
#include "../include/gestores/gestor_historico.h"
#include "../include/Queries.h"

#define MAX_SIZE 256

void query1_interativo(GHashTable *utilizador_table, GHashTable *artista_table, GHashTable *musicas_table) {
    clear();

    // Cria a borda com box()
    int start_x = 2, start_y = 1;
    int box_width = COLS - 4;
    int box_height = LINES - 6;

    WINDOW *win = newwin(box_height, box_width, start_y, start_x);
    box(win, 0, 0);
    wrefresh(win);

    // Título
    mvprintw(start_y, start_x + 2, " Query 1: Listar resumo de um utilizador ou artista ");

    // Conteúdo dentro da box
    mvprintw(start_y + 2, start_x + 2, "Comando:");
    mvprintw(start_y + 3, start_x + 4, "1 <ID>");

    mvprintw(start_y + 5, start_x + 2, "Output:");
    mvprintw(start_y + 7, start_x + 4, "Utilizador:");
    mvprintw(start_y + 8, start_x + 6, "email;first_name;last_name;age;country");
    mvprintw(start_y + 10, start_x + 4, "Artista:");
    mvprintw(start_y + 11, start_x + 6, "name;type;country;num_albums_individual;total_recipe");

    // Atualiza a tela
    refresh();

    // Inputs para número inteiro e ID
    char id[MAX_SIZE];
    if(utilizador_table == NULL){
      printf("Erro: Tabela de utilizadores não foi carregada.\n");
      return;
    }

    // Recebe o identificador (ID)
    mvprintw(start_y + box_height + 2, 2, "Digite o identificador (ID): ");
    echo(); // Ativa entrada visível
    curs_set(TRUE); // Exibe o cursor
    getnstr(id, sizeof(id) - 1); // Lê o ID

    gint has_S;
    gchar resposta[MAX_SIZE];
    mvprintw(start_y + box_height + 4, 2, "Deseja incluir 'S'? (Digite 'S'(sim) ou 'N'(não)): ");
    echo();
    curs_set(TRUE);
    getnstr(resposta, sizeof(resposta) - 1);
    noecho();
    curs_set(FALSE);

    if(strcmp(resposta,"S") == 0){
        has_S = 1;
    }
    else{
        has_S = 0;
    }

    noecho();
    curs_set(FALSE); 
    clear();
    mvprintw(2, 2, "Valores recebidos:");
    mvprintw(3, 4, "Identificador (ID): %s", id);
    //aplicar aqui a query1 com os valores recebidos
    gchar *output = Querie1(utilizador_table,artista_table,musicas_table, id,has_S);

     if (output == NULL) {
        mvprintw(2, 2, "Erro: Nenhum resultado encontrado para o ID '%s'.", id);
    } else {

        // Exibe a saída formatada
        char *line = strtok(output, "\n"); // Divide o output em linhas
        int line_num = 4;
        while (line != NULL && line_num < LINES - 2) {
            mvprintw(5, 4, "%s", output);
            line = strtok(NULL, "\n");
        }

        g_free(output); // Libera memória alocada pela query
    }

    mvprintw(LINES - 2, 2, "Pressione qualquer tecla duas vezes para voltar ao menu.");
    refresh();
    getch();

    // Libera a memória da janela
    delwin(win);
}



void query2_interativo(DiscoArtist **discografiaArt, GHashTable *artistas) {
    clear();

    // Cria a borda com box()
    int start_x = 2, start_y = 1;
    int box_width = COLS - 4;
    int box_height = LINES - 6;

    WINDOW *win = newwin(box_height, box_width, start_y, start_x);
    box(win, 0, 0);
    wrefresh(win);

    // Título
    mvprintw(start_y, start_x + 2, " Query 2: Quais são os top N artistas com maior discografia? ");

    // Instruções
    mvprintw(start_y + 2, start_x + 2, "Comando:");
    mvprintw(start_y + 3, start_x + 4, "2 <N> [country]");
    mvprintw(start_y + 5, start_x + 2, "Output:");
    refresh();

    // Inputs para número de artistas (N) e país (opcional)
    int N;
    char country[MAX_SIZE];
    mvprintw(start_y + box_height - 3, 2, "Digite o número de artistas (N): ");
    echo();
    curs_set(TRUE);
    scanw("%d", &N);
    mvprintw(start_y + box_height - 2, 2, "Digite o país (opcional, pressione Enter para ignorar): ");
    getnstr(country, sizeof(country) - 1);
    noecho();
    curs_set(FALSE);

    gint has_S;
    gchar resposta[MAX_SIZE];
    mvprintw(start_y + box_height , 2, "Deseja incluir 'S'? (Digite 'S'(sim) ou 'N'(não)): ");
    echo();
    curs_set(TRUE);
    getnstr(resposta, sizeof(resposta) - 1);
    noecho();
    curs_set(FALSE);

    if(strcmp(resposta,"S") == 0){
        has_S = 1;
    }
    else{
        has_S = 0;
    }


    clear();

    // Executa a query e exibe o resultado
    gchar **resultados = Querie2(discografiaArt, artistas, N, strlen(country) > 0 ? country : NULL,has_S);
    if (resultados == NULL) {
        mvprintw(2, 2, "Erro: Nenhum resultado encontrado.");
    } else {
        mvprintw(2, 2, "Top %d artistas%s%s:", N, strlen(country) > 0 ? " do país " : "", country);
        for (int i = 0; resultados[i] != NULL && i < N; i++) {
            mvprintw(4 + i, 4, "%s", resultados[i]);
            g_free(resultados[i]); // Libera cada string individual
        }
        g_free(resultados); // Libera o array
    }

    mvprintw(LINES - 2, 2, "Pressione qualquer tecla para voltar ao menu.");
    refresh();
    getch();

    // Libera a memória da janela
    delwin(win);
}



void query3_interativo(GenreLikes ***gL) {
    clear();

    // Cria a borda com box()
    int start_x = 2, start_y = 1;
    int box_width = COLS - 4;
    int box_height = LINES - 6;

    WINDOW *win = newwin(box_height, box_width, start_y, start_x);
    box(win, 0, 0);
    wrefresh(win);

    // Título
    mvprintw(start_y, start_x + 2, " Query 3: Quais são os gêneros de música mais populares? ");

    // Instruções
    mvprintw(start_y + 2, start_x + 2, "Comando:");
    mvprintw(start_y + 3, start_x + 4, "3 <min age> <max age>");
    mvprintw(start_y + 5, start_x + 2, "Output:");
    refresh();

    // Inputs para faixa etária
    int min_age, max_age;
    mvprintw(start_y + box_height - 3, 2, "Digite a idade mínima: ");
    echo();
    curs_set(TRUE);
    scanw("%d", &min_age);
    mvprintw(start_y + box_height - 2, 2, "Digite a idade máxima: ");
    scanw("%d", &max_age);
    noecho();
    curs_set(FALSE);

    gint has_S;
    gchar resposta[MAX_SIZE];
    mvprintw(start_y + box_height , 2, "Deseja incluir 'S'? (Digite 'S'(sim) ou 'N'(não)): ");
    echo();
    curs_set(TRUE);
    getnstr(resposta, sizeof(resposta) - 1);
    noecho();
    curs_set(FALSE);

    if(strcmp(resposta,"S") == 0){
        has_S = 1;
    }
    else{
        has_S = 0;
    }

    clear();

    // Executa a query e exibe o resultado
    gchar **resultado = Querie3(gL, min_age, max_age, has_S);
    if (resultado == NULL) {
        mvprintw(2, 2, "Erro: Nenhum resultado encontrado.");
    } else {
        mvprintw(2, 2, "Gêneros de música mais populares (idade %d-%d):", min_age, max_age);
        int line_num = 4;
        for (int i = 0; resultado[i] != NULL && line_num < LINES - 2; i++) {
            mvprintw(line_num++, 4, "%s", resultado[i]);
            g_free(resultado[i]); // Libera cada string individual
        }
        g_free(resultado); // Libera o array
    }

    mvprintw(LINES - 2, 2, "Pressione qualquer tecla para voltar ao menu.");
    refresh();
    getch();

    // Libera a memória da janela
    delwin(win);
}


void query4_interativo(GHashTable **cumulativeRanking, GHashTable *artistas) {
    clear();

    // Cria a borda com box()
    int start_x = 2, start_y = 1;
    int box_width = COLS - 4;
    int box_height = LINES - 6;

    WINDOW *win = newwin(box_height, box_width, start_y, start_x);
    box(win, 0, 0);
    wrefresh(win);

    // Título
    mvprintw(start_y, start_x + 2, " Query 4: Artista mais popular em um intervalo de datas ");

    // Instruções
    mvprintw(start_y + 2, start_x + 2, "Comando:");
    mvprintw(start_y + 3, start_x + 4, "4 <data inicial> <data final>");
    mvprintw(start_y + 5, start_x + 2, "Output:");
    refresh();

    // Inputs para as datas
    char data_inicial[11] = {0}, data_final[11] = {0};  // Inicializa como vazio
    mvprintw(start_y + box_height - 4, 2, "Digite a data inicial (AAAA/MM/DD) ou pressione Enter se não receber: ");
    echo();
    curs_set(TRUE);
    getnstr(data_inicial, 10);
    
    if (data_inicial[0] == '\0') {  // Se o campo da data inicial estiver vazio, define como NULL
        data_inicial[0] = '\0';  // Explicitamente define como NULL (vazio)
    }

    mvprintw(start_y + box_height - 3, 2, "Digite a data final (AAAA/MM/DD) ou pressione Enter se não receber : ");
    getnstr(data_final, 10);

    if (data_final[0] == '\0') {  // Se o campo da data final estiver vazio, define como NULL
        data_final[0] = '\0';  // Explicitamente define como NULL (vazio)
    }
    
    noecho();
    curs_set(FALSE);

    gint has_S;
    gchar resposta[MAX_SIZE];
    mvprintw(start_y + box_height , 2, "Deseja incluir 'S'? (Digite 'S'(sim) ou 'N'(não)): ");
    echo();
    curs_set(TRUE);
    getnstr(resposta, sizeof(resposta) - 1);
    noecho();
    curs_set(FALSE);

    if(strcmp(resposta,"S") == 0){
        has_S = 1;
    }
    else{
        has_S = 0;
    }


    clear();

    // Verifica se as datas são NULL ou não, e chama a função de query correspondente
    gchar *resultado = NULL;
    if (data_inicial[0] == '\0' && data_final[0] == '\0') {
        // Se não foram fornecidas datas, chama a função com NULL
        resultado = Querie4(NULL, NULL, cumulativeRanking, artistas, has_S);
    } else {
        // Caso contrário, usa as datas fornecidas
        resultado = Querie4(data_inicial, data_final, cumulativeRanking, artistas, has_S);
    }

    if (resultado == NULL) {
        mvprintw(2, 2, "Erro: Nenhum resultado encontrado para o intervalo fornecido.");
    } else {
        mvprintw(2, 2, "Artista mais popular entre %s e %s:", 
                 (data_inicial[0] != '\0') ? data_inicial : "sem data inicial", 
                 (data_final[0] != '\0') ? data_final : "sem data final");

        char *line = strtok(resultado, "\n");
        int line_num = 4;
        while (line != NULL && line_num < LINES - 2) {
            mvprintw(line_num++, 4, "%s", line);
            line = strtok(NULL, "\n");
        }
        g_free(resultado); // Libera a memória da string
    }

    mvprintw(LINES - 2, 2, "Pressione qualquer tecla para voltar ao menu.");
    refresh();
    getch();

    // Libera a memória da janela
    delwin(win);
}


void query5_interativo(GHashTable *user_music_table, int nr_utilizadores,
                       gint **matriz, GHashTable *utilizadores_table,
                       gint *idsUtilizadores, char **nomesGenerosPtr) {
    if (!user_music_table || !matriz || !utilizadores_table || !idsUtilizadores || !nomesGenerosPtr) {
        mvprintw(2, 2, "Erro: Tabelas ou estruturas de dados não inicializadas.");
        refresh();
        getch();
        return;
    }

    while (1) {
        clear();
        mvprintw(2, 2, "Query 5: Recomendações baseadas no recomendador feito.");

        // Pedir ID do utilizador
        char id_utilizador[64] = {0};
        mvprintw(4, 2, "Digite o ID do utilizador alvo:");
        echo();
        curs_set(TRUE);
        getnstr(id_utilizador, sizeof(id_utilizador) - 1);
        gint utilizadorID = g_ascii_strtoll(id_utilizador + 1, NULL, 10);
        noecho();

        // Pedir número de recomendações
        int nr_recomendacoes = 0;
        mvprintw(6, 2, "Digite o número de recomendações (máx: %d):", nr_utilizadores);
        echo();
        curs_set(TRUE);
        scanw("%d", &nr_recomendacoes);
        noecho();

        // Validar número de recomendações
        if (nr_recomendacoes <= 0 || nr_recomendacoes > nr_utilizadores) {
            mvprintw(8, 2, "Número inválido de recomendações.");
            refresh();
            getch();
            continue;
        }

        // Usar o recomendador feito
        gchar **resultado = Querie5_myrecomenda(utilizadorID, nr_recomendacoes,
                                                matriz, utilizadores_table, idsUtilizadores);

        if (!resultado) {
            mvprintw(8, 2, "Erro: Não foi possível gerar recomendações.");
            refresh();
            getch();
            continue;
        }

        // Exibir os resultados
        clear();
        mvprintw(2, 2, "Recomendações:");
        for (int j = 0; j < nr_recomendacoes && resultado[j]; j++) {
            mvprintw(4 + j, 4, "%s", resultado[j]);
        }

        mvprintw(LINES - 2, 2, "Pressione qualquer tecla para voltar ao menu.");
        refresh();
        getch();

        // Liberar memória usada pelas recomendações
        for (int j = 0; j < nr_recomendacoes; j++) {
            g_free(resultado[j]);
        }

        return; // Voltar ao menu principal
    }
}




void query6_interativo(GHashTable *historicos_por_user_table, GHashTable *musicas_table,
                       GHashTable *artistas_table, GHashTable *albuns_table) {
    clear();

    // Cria a borda com box()
    int start_x = 2, start_y = 1;
    int box_width = COLS - 4;
    int box_height = LINES - 6;

    WINDOW *win = newwin(box_height, box_width, start_y, start_x);
    box(win, 0, 0);
    wrefresh(win);

    // Título
    mvprintw(start_y, start_x + 2, " Query 6: Estatísticas de música para um usuário em um ano ");

    // Instruções
    mvprintw(start_y + 2, start_x + 2, "Comando:");
    mvprintw(start_y + 3, start_x + 4, "6 <user_id> <ano> [N]");
    mvprintw(start_y + 5, start_x + 2, "Output:");
    refresh();

    // Inputs para user_id, ano e N
    char user_id[MAX_SIZE] = {0}, year[5] = {0};
    int N = 0;

    mvprintw(start_y + box_height - 4, 2, "Digite o ID do usuário: ");
    echo();
    curs_set(TRUE);
    getnstr(user_id, 255);
    gint *idUser = malloc(sizeof(gint));
    *idUser = g_ascii_strtoll(user_id + 1, NULL, 10);
    
    mvprintw(start_y + box_height - 3, 2, "Digite o ano (AAAA): ");
    getnstr(year, 4);
    
    mvprintw(start_y + box_height - 2, 2, "Digite N (opcional, 0 para todos): ");
    scanw("%d", &N);
    noecho();
    curs_set(FALSE);

    gint has_S;
    gchar resposta[MAX_SIZE];
    mvprintw(start_y + box_height , 2, "Deseja incluir 'S'? (Digite 'S'(sim) ou 'N'(não)): ");
    echo();
    curs_set(TRUE);
    getnstr(resposta, sizeof(resposta) - 1);
    noecho();
    curs_set(FALSE);

    if(strcmp(resposta,"S") == 0){
        has_S = 1;
    }
    else{
        has_S = 0;
    }

    clear();

    // Validar se o user_id e ano foram fornecidos
    if (user_id[0] == '\0' || year[0] == '\0') {
        mvprintw(2, 2, "Erro: ID do usuário ou ano não fornecido.");
    } else {
        gchar **output = NULL;
        gint output_size = 0;

        // Chama a função Querie6 para obter as estatísticas
        Querie6(historicos_por_user_table, musicas_table, artistas_table,
                albuns_table, idUser, year, N, &output, &output_size, has_S);

        if (output_size == 0 || output[0][0] == '\0') {
            mvprintw(2, 2, "Nenhum resultado encontrado para o usuário %s no ano %s.", user_id, year);
        } else {
            mvprintw(2, 2, "Estatísticas para o usuário %s no ano %s:", user_id, year);
            for (int j = 0; j < output_size; j++) {
                mvprintw(4 + j, 4, "%s", output[j]);
            }
            g_free(output[0]); // Libera a memória da string
            g_free(output);    // Libera o vetor de saída
        }
    }

    mvprintw(LINES - 2, 2, "Pressione qualquer tecla para voltar ao menu.");
    refresh();
    getch();

    // Libera a memória da janela
    delwin(win);
}
