import React from 'react'
import ReactDOM from 'react-dom/client'
import { Elm } from './elm/Main.elm'
import App from './App'
import './index.css'
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

console.log(appState);

const network = new Network();

network.on('data', (data) => {
  console.log('data', data);
  appState.ports.handleMsg.send(data);
});

appState.ports.sendMsg.subscribe((data) => {
  console.log('sendMsg', data);
  void network.send(data);
});

appState.ports.joinRoom.subscribe(code => {
  void network.connect(code);
});

network.onReady(code => {
  appState.ports.createRoom.send(code);
});

ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)
