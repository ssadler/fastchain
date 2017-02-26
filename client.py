import json
import time
import requests
import hashlib


def post(tx):
    txid = hashlib.md5(json.dumps(tx)).hexdigest()
    tx['id'] = txid
    print tx
    res = requests.post('http://localhost:8500/transactions', json=tx)
    print(res)
    if res.status_code == 500:
        import pdb; pdb.set_trace()
        1
    return txid

tx = {
    'op': 'CreateApp',
    'sql': open('bitcoin.sql').read(),
    'name': 'bitcoin',
    'ts': str(time.time()),
}

app_id = post(tx)


tx = {
    'op': 'CreateAsset',
    'app': app_id,
    'ts': str(time.time()),
}

asset_id = post(tx)


tx = {
    'op': 'Call',
    'asset': asset_id,
    'proc': 'bitcoin_init',
    'body': {},
    'ts': str(time.time()),
}

input_txid = post(tx)

for i in range(1):
    input_txid = post({
        'op': 'Call',
        'asset': asset_id,
        'proc': 'bitcoin_spend',
        'body': {
            'inputs': [{'txid': input_txid, 'output': 0}],
            'outputs': [{'amount': 1000}],
        },
        'ts': str(time.time()),
    })
