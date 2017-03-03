
drop table if exists outputs;
create table outputs (
    txid varchar(64) not null,
    output int not null,
    amount integer not null,
    spentBy varchar(64),
    unique (txid, output)
);

CREATE OR REPLACE FUNCTION tx_inputs(json)
RETURNS TABLE (txid varchar(64), output integer) AS $$
    select * from json_to_recordset($1->'body'->'inputs') as i (txid varchar(64), output integer);
$$ LANGUAGE SQL;


CREATE OR REPLACE FUNCTION tx_outputs(json)
RETURNS TABLE (output bigint, amount integer) AS $$
    select (row_number() OVER() - 1), amount from json_to_recordset($1->'body'->'outputs') as i (output integer, amount integer);
$$ LANGUAGE SQL;


CREATE OR REPLACE FUNCTION bitcoin_init(json)
RETURNS void AS $$
    insert into outputs (txid, output, amount)
        values ($1->>'id', 0, 1000);
$$ LANGUAGE SQL;


CREATE or replace FUNCTION bitcoin_spend(json)
RETURNS VOID AS $$
DECLARE
    tx alias for $1;
    inAmount integer;
    outAmount integer;
    nSpent integer;
    invalidInput bool;
BEGIN
    select bool_or(amount < 1), sum(amount) into invalidInput, outAmount
    from tx_outputs(tx);
    if invalidInput or outAmount < 1 then
        raise exception 'Invalid outputs';
    end if;

	with spent as (
		update outputs o
            set spentBy = i.txid
            from tx_inputs(tx) i
            where i.txid = o.txid
              and i.output = o.output
              and spentBy is null
			returning amount
	) select sum(amount), count(*) into inAmount, nSpent from spent;

	if nSpent != (select count(*) from tx_inputs(tx)) then
		raise exception 'Invalid inputs or double spend';
	end if;

    if inAmount != outAmount then
		raise exception 'Amount mismatch';
    end if;

    insert into outputs (txid, output, amount)
		(select tx->>'id', output, amount from tx_outputs(tx));
END
$$ LANGUAGE plpgsql;

