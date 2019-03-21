import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import emoji from 'emoji-datasource';
import emojiImg from 'emoji-datasource/img/apple/sheets/32.png'

Elm.Main.init({
  node: document.getElementById('root'),
  flags: [emoji, emojiImg]
});

registerServiceWorker();