import pandas as pd
import mysql.connector
import os
from dotenv import load_dotenv

# ordem das tabelas a carregar para evitar erros devido a chaves estrangeiras
ordem_tabelas = [
    "Localizacao",
    "Stand",
    "Contactos_Stand",
    "Cliente",
    "Contactos_Cliente",
    "Funcionario",
    "Contactos_Funcionario",
    "Veiculo",
    "Aluguer"
]

load_dotenv()

db = mysql.connector.connect(
    host=os.getenv("HOST"),
    user=os.getenv("USER"),
    password=os.getenv("PASSWD"),
    database=os.getenv("DATABASE"),
    ssl_disabled=True  
)

def extrair_csvs():
    print("Extraindo ficheiros CSV...")
    dados = {
        "Veiculo": pd.read_csv("dados/veiculo.csv"),
        "Cliente": pd.read_csv("dados/cliente.csv"),
        "Contactos_Cliente": pd.read_csv("dados/contactoscliente.csv"),
        "Localizacao": pd.read_csv("dados/localizacao.csv"),
        "Stand": pd.read_csv("dados/stand.csv"),
        "Contactos_Stand": pd.read_csv("dados/contactosstand.csv"),
        "Aluguer": pd.read_csv("dados/aluguer.csv"),
        "Funcionario": pd.read_csv("dados/funcionario.csv"),
        "Contactos_Funcionario": pd.read_csv("dados/contactosfuncionario.csv")
    }
    return dados

def transformar_dados(dfs):
    print("Transformando dados...")

    def limpar(df):
        df.columns = df.columns.str.lower().str.strip()

        for colunas in ['datanascimento', 'datainicio', 'datafim']:
            if colunas in df.columns:
                df[colunas] = pd.to_datetime(df[colunas], errors='coerce')
                df[colunas] = df[colunas].dt.strftime('%Y-%m-%d')  
        return df

    return {nome: limpar(df) for nome, df in dfs.items()}


# Função para carregar dados manualmente no MySQL com mysql.connector
def carregar_para_mysql_manual(df, tabela, db):
    cursor = db.cursor()
    colunas = df.columns.tolist()
    placeholders = ", ".join(["%s"] * len(colunas))
    cols_formatted = ", ".join(colunas)

    for _, row in df.iterrows():
        valores = tuple(row[col] for col in colunas)
        sql = f"INSERT INTO {tabela} ({cols_formatted}) VALUES ({placeholders})"
        try:
            cursor.execute(sql, valores)
        except Exception as e:
            print(f"Erro ao inserir na tabela '{tabela}': {e}")
    db.commit()
    cursor.close()

def carregar_para_sql_ordenado(dfs_transformados, db):
    for tabela in ordem_tabelas:
        df = dfs_transformados.get(tabela)
        if df is not None:
            print(f"A carregar a tabela '{tabela}'...")
            try:
                carregar_para_mysql_manual(df, tabela, db)
                print(f"Tabela '{tabela}' carregada com sucesso.")
            except Exception as e:
                print(f"Erro ao carregar '{tabela}': {e}")
        else:
            print(f"Aviso: tabela '{tabela}' não encontrada nos dados.")

def processo_Execucao():
    dados = extrair_csvs()
    dados_transformados = transformar_dados(dados)
    carregar_para_sql_ordenado(dados_transformados, db)

if __name__ == "__main__":
    processo_Execucao()
