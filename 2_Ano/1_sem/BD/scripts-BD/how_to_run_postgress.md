### rodar este programa para meter tabelas no postgres
```bash
psql -h 127.0.0.1 -U postgres -d postgres -f belo_cars_postgres.sql 
```

### e depois :
```bash
python3 parser_postgreSQL.py 
```