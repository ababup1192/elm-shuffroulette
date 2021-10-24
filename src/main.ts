import 'destyle.css'
import './style.css'
import { Elm } from './Main.elm'

Elm.Main.init({ node: document.querySelector('main'), flags: { currentTime: Date.now() } })


