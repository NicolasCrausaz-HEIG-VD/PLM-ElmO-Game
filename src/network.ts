import { DataConnection, Peer } from 'peerjs'
import Emittery from 'emittery'

const PREFIX = 'bGEgYmFuZGUgZGVzIGNyYWNrcyBlc3QgZGUgcmV0b3Vy';

function generateId() {
  const alphabet = 'ABCEDFGHJKLMNPQRSTUVWXYZ';
  const code = Array.from({length: 4}, () => alphabet[Math.floor(Math.random() * alphabet.length)]).join('');
  return `${PREFIX}-${code}`
}

function parseId(id: string) {
  return id.replace(`${PREFIX}-`, '');
}

type NetworkEvent<T> = {
  data: T;
  newConnection: DataConnection;
  removeConnection: DataConnection;
}

export class Network<T> extends Emittery<NetworkEvent<T>> {
  private peer: Peer;
  private connections: DataConnection[] = [];
  private readonly isReady: Promise<void>

  public constructor() {
    super();
    this.peer = new Peer(generateId());
    this.isReady = new Promise<void>((resolve) => {
      this.peer.once('open', () => {
        console.log('Ready', this.peer.id);
        this.peer.on('connection', (conn) => {
          console.log('connection', conn);
          this.addConnection(conn);
        });
        resolve();
      });
    });
  }

  public onReady(cb: (code: string) => void) {
    this.isReady.then(() => cb(parseId(this.peer.id)));
  }

  private addConnection(conn: DataConnection) {
    void this.emit('newConnection', conn);
    this.connections.push(conn);
    conn.once('close', () => {
      console.log('close', conn);
      this.connections = this.connections.filter((c) => c.peer !== conn.peer);
      void this.emit('removeConnection', conn);
    });
    conn.on('data', (data: unknown) => {
      void this.emit('data', data as T);
    });
  }

  public async connect(code: string) {
    await this.isReady;
    const conn = this.peer.connect(`${PREFIX}-${code}`);
    return new Promise<DataConnection>((resolve, reject) => {
      conn.once('open', () => {
        console.log('connected', conn);
        this.addConnection(conn);
        resolve(conn);
      });
      conn.once('error', (err) => {
        console.error('error', err);
        reject(err);
      });
    });
  }

  public async send(data: T) {
    await this.isReady;
    this.connections.forEach((conn) => {
      conn.send(data);
    });
  }

  public getConnections() {
    return this.connections;
  }

  public getPeerId() {
    return parseId(this.peer.id);
  }
}
