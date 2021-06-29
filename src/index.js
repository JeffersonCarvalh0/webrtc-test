import { serializeError } from "serialize-error";
import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});

app.ports.setMediaStream.subscribe(async ({ audio, video, videoElementId }) => {
  try {
    const stream = await navigator.mediaDevices.getUserMedia({ audio, video });
    document.getElementById(videoElementId).srcObject = stream;
  } catch (e) {
    const serializedError = serializeError(e);
    console.error(serializedError);
    app.ports.getMediaError.send(serializedError);
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();