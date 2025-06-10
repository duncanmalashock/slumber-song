import '../assets/style.css'
import Main from '../src/Main.elm'

// Elm app initialization
let app = Main.init({
  node: document.getElementById('app'),
  flags: {
    browserDimensions: {
      x: window.innerWidth,
      y: window.innerHeight
    },
    devicePixelRatio: window.devicePixelRatio,
    currentTimeInMS: Date.now(),
    gameFilename: "./Shadowgate/game.json"
  }
});

// Port to Elm
function sendToElm(tag, payload) {
  app.ports.fromJs.send({
    tag,
    payload
  });
}

// Subscribe to ports from Elm
app.ports.toJs.subscribe((data) => {
  switch (data.tag) {
    case "LoadGameData":
      fetch(data.file)
        .then(res => res.json())
        .then(data => {
          sendToElm("GameDataLoaded", data);
        });
      break;

    case "PlaySound":
      playSound(data.file);
      break;

    default:
      console.error("Unknown port tag sent from Elm to JS:", data);
  }
});

//
// -- PORTS FROM ELM --
//

function playSound(filename) {
  const audio = new Audio(filename);
  audio.play();
  audio.addEventListener("ended", function () {
    audio.currentTime = 0;
  });
}