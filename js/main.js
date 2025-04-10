import '../assets/style.css'
import Main from '../src/Main.elm'

let app = Main.init({
  node: document.getElementById('app')
});

app.ports.toJs.subscribe((data) => {
  switch (data.tag) {
    case "alert":
      alert(data.message);
      break;

    default:
      console.error("Unknown tag from Elm:", data);
  }
});

function sayHi() {
  sendToElm("data", "Hello from JavaScript!");
}
setTimeout(sayHi, 1000);

function sendToElm(tag, payload) {
  app.ports.fromJs.send({
    tag: tag,
    payload: payload
  });
}
