import '../assets/style.css'
import Main from '../src/Main.elm'

let app = Main.init({
  node: document.getElementById('app')
});

app.ports.toJs.subscribe((data) => {
  switch (data.tag) {
    case "roomChanged":
      changeRoom(data.id, data.name, data.exits);
      break;

    default:
      console.error("Unknown tag from Elm:", data);
  }
});

function changeRoom(id, name, exits) {
  document.getElementById('room').setHTMLUnsafe(name);
  document.getElementById('exit').setHTMLUnsafe(exits[0].toRoomId);
}

function sendToElm(tag, payload) {
  app.ports.fromJs.send({
    tag: tag,
    payload: payload
  });
}
