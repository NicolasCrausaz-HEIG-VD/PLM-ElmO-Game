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
  }
}

const appState = Elm.Main.init<Ports>({
  node: document.getElementById('elm-root')!,
});

console.log(appState);

const network = new Network();

// get code from url
const code = new URLSearchParams(window.location.search).get('code');
if (code) {
  network.connect(code);
}

network.on('data', (data) => {
  console.log('data', data);
  appState.ports.handleMsg.send(data);
});

appState.ports.sendMsg.subscribe((data) => {
  console.log('sendMsg', data);
  network.send(data);
});

console.log(network.getPeerId());

ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)
