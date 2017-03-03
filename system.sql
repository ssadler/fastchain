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

CREATE TABLE if not exists assets (
    id varchar(64) PRIMARY KEY,
    app_id varchar(64) not null,
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


CREATE or replace FUNCTION create_asset(json,timestamp with time zone)
RETURNS varchar(64) AS $$
DECLARE
    asset varchar(64);
    app varchar(64);
    pl record;
BEGIN
    select $1->>'id', $1->>'app' into asset, app;
    insert into assets (id, app_id, ts)
        values (asset, app, $2)
        returning id into asset;
    execute 'create schema asset_'||asset;
    for pl in select * from information_schema.tables
                       where table_schema = 'app_'||app LOOP
        execute format('CREATE VIEW asset_%s.%s WITH (security_barrier) AS
                   (SELECT * FROM app_%s.%s WHERE asset = %L)
                   WITH CHECK OPTION', asset, pl.table_name, app, pl.table_name, asset);
    END LOOP;
    return asset;
END
$$ LANGUAGE plpgsql;
