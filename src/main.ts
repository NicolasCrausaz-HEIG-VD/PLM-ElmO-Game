import { Elm } from './elm/Main.elm'
import './index.css'
import './index.scss'
import './Card.scss'
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
  createRoom: {
    send: (code: string) => void;
  }
}

const appState = Elm.Main.init<Ports>({
  node: document.getElementById('elm-root')!,
});

console.log('appState', appState);

const network = new Network();

network.on('data', (data) => {
  console.log('data', data);
  appState.ports?.handleMsg?.send(data);
});

appState.ports?.sendMsg?.subscribe((data) => {
  console.log('sendMsg', data);
  void network.send(data);
});

appState.ports?.joinRoom?.subscribe(code => {
  console.log('joinRoom', code);
  void network.connect(code);
});

network.onReady(code => {
  console.log('onReady', code);
  appState.ports?.createRoom?.send(code);
});
