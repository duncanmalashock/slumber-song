import '../assets/style.css'
import Main from '../src/Main.elm'
import './window.js'

// 
let app = Main.init({
  node: document.getElementById('app')
});

app.ports.playSound && app.ports.playSound.subscribe(name => {
  const audio = new Audio(`/game/sfx/${name}.mp3`);
  audio.play();
});
