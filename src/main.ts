import { Elm } from './elm/Main.elm'
import './game.scss'
import './index.scss'
import './card.scss'
import { Network } from './network'

type Ports = {
  sendMsg: {
    subscribe: (callback: (data: unknown) => void) => void;
  }
  handleMsg: {
    send: (data: unknown) => void;
  },
  joinRoom: {
    subscribe: (callback: (code: string) => void) => void;
  },
  joinedRoom: {
    send: (code: string) => void;
  }
  requestRoomCode: {
    subscribe: (callback: () => void) => void;
  }
  createRoom: {
    send: (code: string) => void;
  }
}

const appState = Elm.Main.init<Ports>({
  node: document.getElementById('elm-root')!,
});

console.log('appState', appState);

const network = new Network({
  middleware(conn) {
    return this.getPeers().length < 4;
  }
});

network.on('data', (data) => {
  console.log('data', data);
  appState.ports?.handleMsg?.send(data);
});

appState.ports?.sendMsg?.subscribe((data) => {
  console.log('sendMsg', data);
  void network.send(data);
});

appState.ports?.joinRoom?.subscribe(async code => {
  await network.connect(code);
  appState.ports?.joinedRoom?.send(code);
});

appState.ports?.requestRoomCode?.subscribe(async () => {
  console.log('requestCreateRoom');
  const code = await network.onReady();
  appState.ports?.createRoom?.send(code);
});
