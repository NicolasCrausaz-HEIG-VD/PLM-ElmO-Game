import React from 'react'
import ReactDOM from 'react-dom/client'
import { Elm } from './elm/Main.elm'
import App from './App'
import './index.css'

const appState = Elm.Main.init({
  node: document.getElementById('elm-root')!,
});

ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)
