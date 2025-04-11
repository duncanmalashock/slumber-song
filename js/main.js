import '../assets/style.css'
import Main from '../src/Main.elm'

let app = Main.init({
  node: document.getElementById('app')
});

app.ports.toJs.subscribe((msgs) => {
  msgs.map((data) => {
    switch (data.tag) {
      case "UpdateRoom":
        changeRoom(data.id, data.name, data.exits);
        break;

      case "PlaySound":
        playSound(data.file);
        break;

      default:
        console.error("Unknown tag from Elm:", data);
    }
  })
});

const goButton = document.getElementById('go');
goButton.addEventListener("mouseup", (e) => { sendToElm("userClickedGoButton", {}) });

function changeRoom(id, name, exits) {
  document.getElementById('room').setHTMLUnsafe(name);

  const oldExitElement = document.getElementById('exit');
  const newExitElement = oldExitElement.cloneNode(true);
  oldExitElement.parentNode.replaceChild(newExitElement, oldExitElement);

  newExitElement.setHTMLUnsafe(exits[0].toRoomId);
  newExitElement.addEventListener("mouseup", (e) => {
    sendToElm("userClickedExit", { toRoomId: exits[0].toRoomId });
  });
}

function playSound(filename) {
  const audio = new Audio(filename);
  audio.play();
}

function sendToElm(tag, payload) {
  app.ports.fromJs.send({
    tag: tag,
    payload: payload
  });
}
