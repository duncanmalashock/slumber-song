import '../assets/style.css'
import Main from '../src/Main.elm'

let app = Main.init({
  node: document.getElementById('app')
});

app.ports.toJs.subscribe((msgs) => {
  msgs.map((data) => {
    switch (data.tag) {
      case "roomChanged":
        changeRoom(data.id, data.name, data.exits);
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
  const exitElement = document.getElementById('exit')
  exitElement.setHTMLUnsafe(exits[0].toRoomId);
  exitElement.addEventListener("mouseup", (e) => { sendToElm("userClickedExit", { toRoomId: exits[0].toRoomId}) });
}

function sendToElm(tag, payload) {
  app.ports.fromJs.send({
    tag: tag,
    payload: payload
  });
}
