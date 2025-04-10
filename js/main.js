import '../assets/style.css'
import Main from '../src/Main.elm'

let app = Main.init({
  node: document.getElementById('app')
});

app.ports.toJs.subscribe((data) => {
  switch (data.tag) {
    case "roomChanged":
      changeRoom(data.id, data.name);
      alert(`Room changed: id:${data.id}, name:${data.name}`);
      break;

    default:
      console.error("Unknown tag from Elm:", data);
  }
});

function changeRoom(id, name) {
  document.getElementById('room').setHTMLUnsafe(name);
}

function sayHi() {
  sendToElm("data", "Sending to Elm through ports is still working");
}
setTimeout(sayHi, 1000);

function sendToElm(tag, payload) {
  app.ports.fromJs.send({
    tag: tag,
    payload: payload
  });
}
