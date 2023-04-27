import { Elm } from './elm/Main.elm'
import './game.scss'
import './index.scss'
import './card.scss'
import { Network } from './network'

type Ports = {
  joinRoom: {
    subscribe: (callback: ([code, username]:[string, string]) => void) => void;
  },
  joinedRoom: {
    send: ([code, playerUUID]:[string, string]) => void;
  }
  requestRoomCode: {
    subscribe: (callback: (username:string) => void) => void;
  }
  createRoom: {
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


let network: Network<{username: string}> | undefined;

// Client
appState.ports?.joinRoom?.subscribe(async ([code, username]) => {
  network = new Network({
    metadata: {
      username
    }
  });
  await network.connect(code);

  network.channel('data').on(data => {
    appState.ports?.incomingData?.send(data);
  });

  appState.ports?.outgoingAction?.subscribe((action) => {
    network?.channel('action').send(code, action);
  });

  appState.ports?.joinedRoom?.send([code, network.getPeerId()]);
});


// Host
appState.ports?.requestRoomCode?.subscribe(async username => {
  network = new Network({
    metadata: {
      username
    },
    middleware() {
      return this.getPeers().length < 4;
    }
  });
  const code = await network.onReady();

  network.on('newConnection', ({metadata,conn}) => {
    appState.ports?.incomingAction?.send({
      action: 'playerJoin',
      uuid: conn.peer,
      name: metadata.username,
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

  appState.ports?.createRoom?.send(code);

  appState.ports?.incomingAction?.send({
    action: 'playerJoin',
    uuid: network.getPeerId(),
    name: username,
  });
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
  }>;
  currentPlayer: string;
  drawStack: number;
  activeCard: string;
  activeColor: string;
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
  };
  currentPlayer: string;
  drawStack: number;
  activeCard: string;
  activeColor: string;
}

function hostToClient(host: HostGameState, localPlayerUUID: string): ClientGameState {
  return {
    distantPlayers: host.players.filter(player => player.uuid !== localPlayerUUID).map(player => ({
      name: player.name,
      uuid: player.uuid,
      cards: player.hand.length,
    })),
    localPlayer: host.players.find(player => player.uuid === localPlayerUUID)!,
    currentPlayer: host.currentPlayer,
    drawStack: host.drawStack,
    activeCard: host.activeCard,
    activeColor: host.activeColor,
  }
}