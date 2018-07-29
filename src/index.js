import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'), localStorage.getItem("model"));

document.addEventListener("mouseup", mouseEvent => {
    const parent = document.getElementById("js-area");
    if (!parent) {
        return null;
    }
    const parentPosition = parent.getBoundingClientRect();
    app.ports.mouseUp.send({
        finalPosition: {x: mouseEvent.clientX, y: mouseEvent.clientY},
        parentPosition: {x: parentPosition.left, y: parentPosition.top}
    });
});

app.ports.toSaveModule.subscribe(modelToSave => {
    localStorage.setItem("model", modelToSave)
});

registerServiceWorker();
