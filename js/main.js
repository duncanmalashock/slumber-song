import '../assets/style.css'
import Main from '../src/Main.elm'

let app = Main.init({
  node: document.getElementById('app')
});

app.ports.toJs.subscribe((data) => {
  const tag = data.tag;

  switch (tag) {
    case "alert":
      alert(data.message);
      break;

    case "data":
      console.log("Received data from Elm:", data.payload);
      break;

    default:
      console.warn("Unknown tag from Elm:", data);
  }
});

// Send message to Elm
function sendToElm() {
  app.ports.fromJs.send({
    tag: "data",
    payload: "Hello from JavaScript!"
  });
}

setTimeout(sendToElm, 1000);