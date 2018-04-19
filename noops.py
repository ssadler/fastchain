import gevent.monkey
gevent.monkey.patch_all()
import gevent
import sys
import ujson as json
import time
import hashlib
import socket
from gevent.pool import Pool


headers = ("POST /transactions HTTP/1.1\r\n"
           "Host: 127.0.0.1:8500\r\n"
           "User-Agent: Mozilla/5.0\r\n"
           "Connection: Close\r\n"
           "Content-type: application/json\r\n"
           "Content-Length:")


def post(tx):
    txid = hashlib.md5(json.dumps(tx)).hexdigest()
    tx['id'] = txid
    body = json.dumps(tx)
    req = headers + "%s\r\n\r\n%s" % (len(body), body)
    conn = socket.socket()
    conn.connect(('127.0.0.1', 8500))
    assert len(req) == conn.send(req)
    conn.close()


pool = Pool(10)

for i in range(int(sys.argv[1])):
    pool.spawn(post, {
        'op': 'Noop',
        'data': [],
        'ts': str(time.time()),
    })

pool.join()
