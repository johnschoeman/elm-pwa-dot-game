import './output.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';


let rawData = localStorage.getItem("blackDotJump.levels")

const parseData = (data) => {
  return data
    ? data.split(",").map((el) => el === "true") 
    : new Array(30).fill(false)
}

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: parseData(rawData)
});

app.ports.saveLevels.subscribe((data) => {
  localStorage.setItem("blackDotJump.levels", data)
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
