import mysql.connector
import json
import os
from dotenv import load_dotenv

load_dotenv()

db = mysql.connector.connect(
    host=os.getenv("HOST"),
    user=os.getenv("USER"),
    password=os.getenv("PASSWD"),
    database=os.getenv("DATABASE"),
    ssl_disabled=True  
)

cursor = db.cursor()

# Diretório onde estão os ficheiros JSON
MODEL_DIR = os.path.join(os.path.dirname(__file__), "model")

# Funções de inserção mapeadas
insert_map = {
    "localizacao": lambda l: cursor.execute(
        "INSERT INTO Localizacao (ID, Endereco, CodigoPostal) VALUES (%s, %s, %s)",
        (l["Id"], l["Endereco"], l["CodigoPostal"])
    ),
    "stand": lambda s: cursor.execute(
        "INSERT INTO Stand (ID, Nome, NIF, ID_Localizacao) VALUES (%s, %s, %s, %s)",
        (s["Id"], s["Nome"], s["NIF"], s["ID_Localizacao"])
    ),
    "contactos_stand": lambda cs: cursor.execute(
        "INSERT INTO Contactos_Stand (ID_Stand, Email, Telefone) VALUES (%s, %s, %s)",
        (cs["ID_Stand"], cs["Email"], cs["Telefone"])
    ),
    "cliente": lambda c: cursor.execute(
        "INSERT INTO Cliente (ID, Nome, DataNascimento, NIF) VALUES (%s, %s, %s, %s)",
        (c["ID"], c["Nome"], c["DataNascimento"], c["NIF"])
    ),
    "contactos_cliente": lambda cc: cursor.execute(
        "INSERT INTO Contactos_Cliente (ID_Cliente, Email, Telefone) VALUES (%s, %s, %s)",
        (cc["ID_Cliente"], cc["Email"], cc["Telefone"])
    ),
    "funcionario": lambda f: cursor.execute(
        "INSERT INTO Funcionario (ID, Departamento, Nome, Funcao, ID_Stand) VALUES (%s, %s, %s, %s, %s)",
        (f["ID"], f["Departamento"], f["Nome"], f["Funcao"], f["ID_Stand"])
    ),
    "contactos_funcionario": lambda cf: cursor.execute(
        "INSERT INTO Contactos_Funcionario (ID_Funcionario, Email, Telefone) VALUES (%s, %s, %s)",
        (cf["ID_Funcionario"], cf["Email"], cf["Telefone"])
    ),
    "veiculo": lambda v: cursor.execute(
        """INSERT INTO Veiculo 
           (Matricula, NPassageiros, Tara, CargaMaxima, Transmissao, Combustivel, Consumo,
            Modelo, Ano, Marca, Disponivel, PrecoDiario, Tipo, ID_Stand)
           VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)""",
        (
            v["Matricula"], v["NPassageiros"], v["Tara"], v["CargaMaxima"], v["Transmissao"],
            v["Combustivel"], v["Consumo"], v["Modelo"], v["Ano"], v["Marca"],
            int(v["Disponivel"]), v["PrecoDiario"], v["Tipo"], v["ID_Stand"]
        )
    ),
    "aluguer": lambda a: cursor.execute(
        """INSERT INTO Aluguer 
           (ID, Matricula, MetodoPagamento, ID_Cliente, DataInicio, DataFim, ID_Funcionario, Valor)
           VALUES (%s, %s, %s, %s, %s, %s, %s, %s)""",
        (
            a["ID"], a["Matricula"], a["MetodoPagamento"], a["ID_Cliente"],
            a["DataInicio"], a["DataFim"], a["ID_Funcionario"], a["Valor"]
        )
    ),


}

def load_and_insert():
    """
    Loads JSON data from files and inserts it into a database using predefined functions.

    This function iterates over a mapping of keys to insertion functions (`insert_map`),
    constructs file paths for JSON files based on the keys, and processes the files if they exist.
    For each file, it loads the JSON data, retrieves the list of items associated with the key,
    and attempts to insert each item into the database using the corresponding insertion function.

    If a file does not exist, it is skipped. Errors during file reading or data insertion are
    caught and logged to the console.

    Exceptions:
        - Prints an error message if a JSON file cannot be read.
        - Prints an error message if an item cannot be inserted into the database.

    Requirements:
        - `insert_map`: A dictionary mapping keys to insertion functions.
        - `MODEL_DIR`: A directory path where JSON files are located.
        - JSON files must be named as `<key>.json` and contain data structured with the key as the root.

    Example JSON structure:
        {
            "key": [
                { ... },  # Item 1
                { ... }   # Item 2
            ]
        }
    """
    for key, insert_fn in insert_map.items():
        file_path = os.path.join(MODEL_DIR, f"{key}.json")
        if not os.path.exists(file_path):
            continue
        with open(file_path, encoding="utf-8") as f:
            try:
                data = json.load(f)
                for item in data.get(key, []):
                    try:
                        insert_fn(item)
                    except mysql.connector.Error as e:
                        print(f"Erro ao inserir em {key}: {e}")
            except Exception as e:
                print(f"Erro ao ler {key}.json: {e}")

def main():
    try:
        load_and_insert()
        db.commit()
        print("Dados inseridos com sucesso!")
    except Exception as e:
        db.rollback()
        print("Erro geral:", e)
    finally:
        cursor.close()
        db.close()

if __name__ == "__main__":
    main()
