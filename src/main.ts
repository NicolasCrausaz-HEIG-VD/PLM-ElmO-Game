import { Elm } from './elm/Main.elm'
import './style/index.scss'
import './style/elmo.scss'
import { Network } from './script/network'

type Ports = {
  joinRoom: {
    subscribe: (callback: (code:string) => void) => void;
  },
  joinedRoom: {
    send: ([code, playerUUID, success]:[string, string, boolean]) => void;
  }
  createRoom: {
    subscribe: (callback: () => void) => void;
  }
  createdRoom: {
    send: (code: string) => void;
  }
  outgoingData: {
    subscribe: (callback: (data: HostGameState) => void) => void;
  }
  incomingData: {
    send: (data: ClientGameState) => void;
  }
  incomingAction: {
    send: (action: Action) => void;
  }
  outgoingAction: {
    subscribe: (callback: (data: Action) => void) => void;
  }
}

const appState = Elm.Main.init<Ports>({
  node: document.getElementById('elm-root')!,
});


let network: Network | undefined;

// Client
appState.ports?.joinRoom?.subscribe(async (code) => {
  network = new Network();
  try {
    await network.connect(code);

    network.channel('data').on(data => {
      appState.ports?.incomingData?.send(data);
    });
  
    appState.ports?.outgoingAction?.subscribe((action) => {
      network?.channel('action').send(code, action);
    });
  
    appState.ports?.joinedRoom?.send([code, network.getPeerId(), true]);
  } catch (err) {
    appState.ports?.joinedRoom?.send([code, network.getPeerId(), false]);
  }
});


// Host
appState.ports?.createRoom?.subscribe(async () => {
  network = new Network({
    middleware() {
      return this.getPeers().length < 10;
    }
  });
  const code = await network.onReady();

  network.on('removeConnection', (conn) => {
    console.log('removeConnection', conn);
    appState.ports?.incomingAction?.send({
      action: 'playerLeave',
      uuid: conn.peer,
    });
  });

  network.channel('action').on(data => {
    appState.ports?.incomingAction?.send(data);
  });

  network.channel('data').on(data => {
    appState.ports?.incomingData?.send(data);
  });

  appState.ports?.outgoingData?.subscribe((data) => {
    data.players.forEach(player => {
      network?.channel('data').send(player.uuid, hostToClient(data, player.uuid));
    });
  });

  appState.ports?.outgoingAction?.subscribe((action) => {
    network?.channel('action').send(code, action);
  });

  appState.ports?.createdRoom?.send(code);
});




type Action = {
  action: 'playerJoin';
  uuid: string;
  name: string;
} | unknown;

interface HostGameState {
  players: Array<{
    name: string;
    uuid: string;
    hand: string[];
    saidUno: boolean;
    isAI: boolean;
  }>;
  currentPlayer: string;
  drawStack: number;
  activeCard: string;
  activeColor: string;
  gameOver: boolean;
}

interface ClientGameState {
  distantPlayers: Array<{
    name: string;
    uuid: string;
    cards: number;
  }>;
  localPlayer: {
    name: string;
    uuid: string;
    hand: string[];
    saidUno: boolean;
  };
  currentPlayer: string;
  drawStack: number;
  activeCard: string;
  activeColor: string;
  gameOver: boolean;
}

function hostToClient(host: HostGameState, localPlayerUUID: string): ClientGameState {
  const players = rotateArray(host.players, localPlayerUUID);
  return {
    distantPlayers: players.slice(1).map(player => ({
      name: player.name,
      uuid: player.uuid,
      cards: player.hand.length,
    })),
    localPlayer: players[0],
    currentPlayer: host.currentPlayer,
    drawStack: host.drawStack,
    activeCard: host.activeCard,
    activeColor: host.activeColor,
    gameOver: host.gameOver
  }
}

// function to rotate an array to always have the local player at the start
function rotateArray<T extends { uuid: string }>(arr: T[], localPlayerUUID: string): T[] {
  const index = arr.findIndex(item => item.uuid === localPlayerUUID);
  return [...arr.slice(index), ...arr.slice(0, index)];
}
