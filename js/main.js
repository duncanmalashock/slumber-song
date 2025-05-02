import '../assets/style.css'
import Main from '../src/Main.elm'

// Flag to disable imperative canvas drawing for game logic debugging
let useVDomInterface = true;

// Set up DOM elements
if (!useVDomInterface) {
  const roomDiv = document.createElement("div");
  roomDiv.id = "room";
  
  const goDiv = document.createElement("div");
  goDiv.id = "go";
  goDiv.textContent = "Go";
  
  const exitDiv = document.createElement("div");
  exitDiv.id = "exit";

  const canvas = document.createElement("canvas");
  canvas.id = "screen";
  canvas.width = 512;
  canvas.height = 342;
  
  const body = document.body;
  
  // Insert in reverse order to maintain correct top-down order
  body.insertBefore(exitDiv, body.firstChild);
  body.insertBefore(goDiv, body.firstChild);
  body.insertBefore(roomDiv, body.firstChild);
  body.insertBefore(canvas, body.firstChild);

  goDiv.addEventListener("mouseup", (e) => { sendToElm("UserClickedCommandButton", { command: "go" }) });
}

// Elm app initialization
let app = Main.init({
  node: document.getElementById('app'),
  flags: {
    browserDimensions: {
      x: window.innerWidth,
      y: window.innerHeight
    },
    devicePixelRatio: window.devicePixelRatio,
    currentTimeInMS: Date.now()
  }
});

// Port to Elm
function sendToElm(tag, payload) {
  app.ports.fromJs.send({
    tag: tag,
    payload: payload
  });
}

// Set global interaction lock
// (prevents sending messages during side effects)
let interactionLock = false;

// Subscribe to ports from Elm
app.ports.toJs.subscribe((msgs) => {
  msgs.map((data) => {
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

      case "HighlightCommand":
        highlightCommand(data.command);
        break;

      case "HighlightObject":
        highlightObject(data.objId);
        break;

      case "PrintText":
        printText(data.text);
        break;

      case "ReportError":
        console.error(data.message);
        break;

      case "SaveGameData":
        console.log(data.gameData);
        break;

      default:
        console.error("Unknown port tag sent from Elm to JS:", data);
    }
  })
});

//
// -- PORTS FROM ELM --
//

function playSound(filename) {
  console.log(`EFFECT: playSound ${filename}`);
  interactionLock = true;
  const audio = new Audio(filename);
  audio.play();
  audio.addEventListener("ended", function(){
    audio.currentTime = 0;
    console.log("Sound playback ended");
    interactionLock = false;
  });
}

function highlightCommand(commandName) {
  console.log(`EFFECT: highlightCommand ${commandName}`);
}

function highlightObject(objId) {
  console.log(`EFFECT: highlightObject ${objId}`);
}

function printText(text) {
  console.log(`EFFECT: printText ${text}`);
}

