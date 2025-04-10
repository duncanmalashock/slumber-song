import './assets/style.css'
import Main from './src/Main.elm'

let app = Main.init({
  node: document.getElementById('app')
});

app.ports.playSound && app.ports.playSound.subscribe(name => {
  const audio = new Audio(`sfx/${name}.mp3`);
  audio.play();
});