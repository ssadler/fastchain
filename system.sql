-- execute_ conn "drop table if exists transactions"    
-- execute_ conn "create table transactions (\          
--                 \ seq serial, \                      
--                 \ txid varchar(64) not null unique, \
--                 \ app varchar(64) not null, \        
--                 \ body json not null, \              
--                 \ ts timestamp with time zone) "     

CREATE TABLE if not exists apps (
    id varchar(64) PRIMARY KEY,
    ts timestamp with time zone not null
);

CREATE or replace FUNCTION create_app(json,timestamp with time zone)
RETURNS void AS $$
DECLARE
BEGIN
    insert into apps (id, ts) values ($1->>'id', $2);
    execute 'create schema app_'||($1->>'id');
END
$$ LANGUAGE plpgsql;


create or replace function create_app_triggers(varchar(64))
returns void as $$
begin
    for row in SELECT * FROM information_schema.tables where table_schema = $1

    end loop;

