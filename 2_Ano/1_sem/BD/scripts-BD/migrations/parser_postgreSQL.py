#!/usr/bin/env python3
"""
Script para migrar dados de PostgreSQL para MySQL
Requer: pip install psycopg2-binary mysql-connector-python pandas
"""

import psycopg2
import mysql.connector
import pandas as pd
from typing import List, Dict
import logging

# Configuração de logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class DatabaseMigrator:
    def __init__(self, postgres_config: Dict, mysql_config: Dict):
        self.postgres_config = postgres_config
        self.mysql_config = mysql_config
        self.pg_conn = None
        self.mysql_conn = None
    
    def connect_databases(self):
        try:
            self.pg_conn = psycopg2.connect(**self.postgres_config)
            logger.info("Conectado ao PostgreSQL com sucesso")
            
            self.mysql_conn = mysql.connector.connect(**self.mysql_config)
            logger.info("Conectado ao MySQL com sucesso")
        except Exception as e:
            logger.error(f"Erro ao conectar às bases de dados: {e}")
            raise
    
    def get_postgres_tables(self) -> List[str]:
        """Obtém lista de tabelas do PostgreSQL com nomes respeitando maiúsculas/minúsculas"""
        try:
            cursor = self.pg_conn.cursor()
            cursor.execute("""
                SELECT c.relname
                FROM pg_class c
                JOIN pg_namespace n ON n.oid = c.relnamespace
                WHERE c.relkind = 'r' AND n.nspname = 'public'
            """)
            tables = [row[0] for row in cursor.fetchall()]
            cursor.close()
            return tables
        except Exception as e:
                logger.error(f"Erro ao obter tabelas do PostgreSQL: {e}")
        raise

    
    def get_table_schema(self, table_name: str) -> List[Dict]:
        try:
            cursor = self.pg_conn.cursor()
            # Aspas duplas para preservar case sensitivity no PostgreSQL
            cursor.execute(f"""
                SELECT column_name, data_type, is_nullable, column_default
                FROM information_schema.columns
                WHERE table_name = %s AND table_schema = 'public'
                ORDER BY ordinal_position
            """, (table_name,))
            
            columns = []
            for row in cursor.fetchall():
                columns.append({
                    'name': row[0],
                    'type': row[1],
                    'nullable': row[2] == 'YES',
                    'default': row[3]
                })
            cursor.close()
            return columns
        except Exception as e:
            logger.error(f"Erro ao obter esquema da tabela {table_name}: {e}")
            raise
    
    def convert_postgres_to_mysql_type(self, pg_type: str) -> str:
        type_mapping = {
            'integer': 'INT',
            'bigint': 'BIGINT',
            'smallint': 'SMALLINT',
            'numeric': 'DECIMAL',
            'real': 'FLOAT',
            'double precision': 'DOUBLE',
            'character varying': 'VARCHAR(255)',
            'varchar': 'VARCHAR(255)',
            'text': 'TEXT',
            'char': 'CHAR',
            'boolean': 'TINYINT(1)',
            'date': 'DATE',
            'timestamp without time zone': 'DATETIME',
            'timestamp with time zone': 'TIMESTAMP',
            'time': 'TIME',
            'bytea': 'BLOB'
        }
        
        if '(' in pg_type:
            base_type = pg_type.split('(')[0]
            if base_type in type_mapping:
                if base_type == 'character varying':
                    return pg_type.replace('character varying', 'VARCHAR')
                return type_mapping[base_type]
        
        return type_mapping.get(pg_type, 'TEXT')
    
    def create_mysql_table(self, table_name: str, columns: List[Dict]):
        try:
            cursor = self.mysql_conn.cursor()
            column_definitions = []
            for col in columns:
                mysql_type = self.convert_postgres_to_mysql_type(col['type'])
                nullable = "NULL" if col['nullable'] else "NOT NULL"
                col_def = f"  {col['name']} {mysql_type} {nullable}"
                column_definitions.append(col_def)


            create_sql = f"CREATE TABLE IF NOT EXISTS {table_name} (\n"
            create_sql += ",\n".join(column_definitions)
            create_sql += "\n) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4"

            cursor.execute(create_sql)
            self.mysql_conn.commit()
            cursor.close()
            logger.info(f"Tabela {table_name} criada no MySQL")
        except Exception as e:
            logger.error(f"Erro ao criar tabela {table_name} no MySQL: {e}")
            raise
    
    def migrate_table_data(self, table_name: str, batch_size: int = 1000):
        try:
            offset = 0
            total_rows = 0
            
            while True:
                # Aspas duplas para o nome da tabela no PostgreSQL (case sensitive)
                query = f'SELECT * FROM "{table_name}" LIMIT {batch_size} OFFSET {offset}'
                df = pd.read_sql(query, self.pg_conn)
                if df.empty:
                    break
                
                cursor = self.mysql_conn.cursor()
                
                columns = list(df.columns)
                placeholders = ', '.join(['%s'] * len(columns))
                insert_sql = f"INSERT INTO `{table_name}` ({', '.join([f'`{col}`' for col in columns])}) VALUES ({placeholders})"
                
                data = []
                for _, row in df.iterrows():
                    row_data = []
                    for value in row:
                        if pd.isna(value):
                            row_data.append(None)
                        elif isinstance(value, pd.Timestamp):
                            row_data.append(value.to_pydatetime())
                        else:
                            row_data.append(value)
                    data.append(tuple(row_data))
                
                cursor.executemany(insert_sql, data)
                self.mysql_conn.commit()
                cursor.close()
                
                total_rows += len(df)
                offset += batch_size
                logger.info(f"Migrados {total_rows} registros da tabela {table_name}")
            
            logger.info(f"Migração da tabela {table_name} concluída. Total: {total_rows} registros")
        except Exception as e:
            logger.error(f"Erro ao migrar dados da tabela {table_name}: {e}")
            raise
    
    def migrate_database(self, tables: List[str] = None, batch_size: int = 1000):
        try:
            self.connect_databases()
            
            if tables is None:
                tables = self.get_postgres_tables()
            
            logger.info(f"Iniciando migração de {len(tables)} tabelas")
            
            for table_name in tables:
                logger.info(f"Migrando tabela: {table_name}")
                
                schema = self.get_table_schema(table_name)
                
                self.create_mysql_table(table_name, schema)
                
                self.migrate_table_data(table_name, batch_size)
            
            logger.info("Migração concluída com sucesso!")
        except Exception as e:
            logger.error(f"Erro durante a migração: {e}")
            raise
        finally:
            self.close_connections()
    
    def close_connections(self):
        if self.pg_conn:
            self.pg_conn.close()
            logger.info("Conexão PostgreSQL fechada")
        
        if self.mysql_conn:
            self.mysql_conn.close()
            logger.info("Conexão MySQL fechada")


def main():
    postgres_config = {
        'host': '127.0.0.1',
        'port': 5432,
        'database': 'postgres',
        'user': 'postgres',
        'password': 'postgres'
    }
    
    mysql_config = {
        'host': '127.0.0.1',
        'port': 3306,
        'database': 'BeloCars',
        'user': 'root',
        'password': 'password',
        'autocommit': False,
        'ssl_disabled':True 
    }
    
    migrator = DatabaseMigrator(postgres_config, mysql_config)
    
    try:
        migrator.migrate_database()
    except Exception as e:
        logger.error(f"Falhou a migração: {e}")


if __name__ == "__main__":
    main()
