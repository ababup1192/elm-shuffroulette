import 'destyle.css'
import './style.css'
import { Elm } from './Main.elm'

const strageKey = "list";
const storedItem = localStorage.getItem(strageKey);

const app = Elm.Main.init({ node: document.querySelector('main'), flags: { currentTime: Date.now(), listValue: storedItem } })

app.ports.saveList.subscribe((list: string) => {
  localStorage.setItem(strageKey, JSON.stringify(list));
});