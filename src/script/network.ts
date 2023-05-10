import { DataConnection, Peer } from 'peerjs'
import Emittery from 'emittery'

type NetworkOptions<TEvent> = {
  middleware?: (this: Network<TEvent>, conn: DataConnection) => boolean;
}

type Message<TEvent = Record<string, any>> = {
  channel: keyof TEvent;
  data: TEvent[keyof TEvent];
}

export class Network<TEvent = Record<string, any>> extends Emittery<{
  newConnection: DataConnection;
  removeConnection: DataConnection;
}> {
  private peer: Peer;
  private connections = new Map<string, DataConnection>();
  private readonly isReady: Promise<void>

  protected channelEmitter = new Emittery<TEvent>();

  protected handleMessage({channel, data}: Message<TEvent>) {
    this.channelEmitter.emit(channel, data);
  }

  public constructor({middleware}: NetworkOptions<TEvent> = {}) {
    super();
    this.peer = new Peer();
    this.isReady = new Promise<void>((resolve) => {
      this.peer.once('open', () => {
        this.peer.on('connection', (conn) => {
          const isAllowed = middleware?.call(this, conn) ?? true;
          if(isAllowed) {
            this.addConnection(conn);
          }else{
            conn.close();
          }
        });
        resolve();
      });
    });
    window.addEventListener('beforeunload', () => {
      this.peer.destroy();
    });
  }

  public async onReady(): Promise<string>;
  public async onReady(cb: (code: string) => void): Promise<string>;
  public async onReady(cb?: (code: string) => void): Promise<string>{
    await this.isReady;
    const code = this.peer.id;
    if (cb) {
      cb(code);
    }
    return code;
  }

  private addConnection(conn: DataConnection) {
    void this.emit('newConnection', conn);

    this.connections.set(conn.peer, conn);
    conn.once('close', () => {
      this.connections.delete(conn.peer);
      void this.emit('removeConnection', conn);
    });
    conn.on('data', (data: unknown) => {
      this.handleMessage(data as Message<TEvent>);
    });
  }

  public async connect(code: string) {
    await this.isReady;
    const conn = this.peer.connect(code,{
      serialization: 'json',
    });
    return new Promise<DataConnection>((resolve, reject) => {
      conn.once('open', () => {
        this.addConnection(conn);
        resolve(conn);
      });
      this.peer.once('error', (err) => {
        reject(err);
      });
    });
  }

  public getConnections() {
    return this.connections;
  }

  public getPeers() {
    return [
      this.peer.id,
      ...Array.from(this.connections.keys())
    ]
  }

  public getPeerId() {
    return this.peer.id;
  }

  public send<TChannel extends keyof TEvent>(peerId: string, channel: TChannel, data: TEvent[TChannel]) {
    if(peerId === this.peer.id) {
      this.channelEmitter.emit(channel, data);
    }else{
      this.connections.get(peerId)?.send({channel, data});
    }
  }

  public channel<TChannel extends keyof TEvent>(channel: TChannel) {
		return {
			on: (cb: (data: TEvent[TChannel]) => void) => this.channelEmitter.on(channel, cb),
			send: (peerId: string, data: TEvent[TChannel]) => {
				this.send(peerId, channel, data);
			},
		};
	}
}
