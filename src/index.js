import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import emoji from 'emoji-datasource';
import emojiImg from 'emoji-datasource/img/apple/sheets/32.png'

Main.embed(document.getElementById('root'), [emoji, emojiImg]);

registerServiceWorker();
