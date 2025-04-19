import '../assets/style.css'
import Main from '../src/Main.elm'

let useVDomInterface = false;

let app = Main.init({
  node: document.getElementById('app'),
  flags: {
    useVDomInterface: useVDomInterface
  }
});

let interactionLock = false;

app.ports.toJs.subscribe((msgs) => {
  msgs.map((data) => {
    switch (data.tag) {
      case "LoadGameData":
        fetch("/game/rooms.json")
          .then(res => res.json())
          .then(data => {
            sendToElm("GameDataLoaded", data);
          });
        break;

      case "UpdateRoom":
        updateRoom(data.id, data.name, data.exits);
        break;

      case "PlaySound":
        playSound(data.file);
        break;

      case "HighlightCommand":
        highlightCommand(data.command);
        break;

      case "ReportError":
        console.error(data.message);
        break;

      default:
        console.error("Unknown port tag sent from Elm to JS:", data);
    }
  })
});

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

  goDiv.addEventListener("mouseup", (e) => { sendToElm("UserClickedGoButton", {}) });
}


function updateRoom(id, name, exits) {
  console.log(`EFFECT: updateRoom ${id}`);
  document.getElementById('room').setHTMLUnsafe(name);

  const oldExitElement = document.getElementById('exit');
  const newExitElement = oldExitElement.cloneNode(true);
  oldExitElement.parentNode.replaceChild(newExitElement, oldExitElement);

  newExitElement.setHTMLUnsafe(exits[0].toRoomId);
  newExitElement.addEventListener("mouseup", (e) => {
    if (interactionLock) {
    }
    else {
      sendToElm("UserClickedExit", { toRoomId: exits[0].toRoomId });
    }
  });
}

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

function sendToElm(tag, payload) {
  app.ports.fromJs.send({
    tag: tag,
    payload: payload
  });
}
