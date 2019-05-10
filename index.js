import { Elm } from './src/Main.elm'

const app = Elm.Main.init({flags: localStorage.getItem("model") || ""});

/* Drag & drop */
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

/* State save */
app.ports.toSaveModule.subscribe(modelToSave => {
    localStorage.setItem("model", modelToSave);
});

/* Export deck */
app.ports.toExportModule.subscribe(modelToExport => {
    const element = document.createElement('a');
    element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(modelToExport));
    element.setAttribute('download', 'deck.json');
    element.style.display = 'none';
    document.body.appendChild(element);
    element.click();
    document.body.removeChild(element);
});

/* Import deck */
app.ports.importDeck.subscribe(function (id) {
    const node = document.getElementById(id);
    if (node === null || node.files.length < 1) {
        return;
    }

    const file = node.files[0];
    const reader = new FileReader();

    reader.onload = (function (event) {
        console.log(event.target.result);
        app.ports.deckImported.send(event.target.result);
    });

    reader.readAsText(file);
});

/* Image selection */
app.ports.fileSelected.subscribe(function (id) {
    const node = document.getElementById(id);
    if (node === null || node.files.length < 1) {
        return;
    }

    const file = node.files[0];
    const reader = new FileReader();

    reader.onload = (function (event) {
        const base64encoded = event.target.result;
        const portData = {
            contents: base64encoded,
            filename: file.name
        };

        app.ports.fileContentRead.send(portData);
    });

    reader.readAsDataURL(file);
});

/* Cropper */
app.ports.toCropper.subscribe(function(data) {
    const url = data.url;
    const size = data.size;
    const resized = data.resized;
    const origin = data.origin;
    const crop = data.crop;
    const canvas = document.createElement('canvas');
    canvas.width = crop.width;
    canvas.height = crop.height;
    const context = canvas.getContext('2d');
    const imageObj = new Image();
    imageObj.crossOrigin = "Anonymous";
    imageObj.src = url;
    imageObj.onerror = function() {
        alert("Image not available for cropping. Probably blocked by CORS policy.");
    };
    imageObj.onload = function() {
        context.drawImage(
            imageObj,
            0,
            0,
            size.width,
            size.height,
            -origin.x,
            -origin.y,
            resized.width,
            resized.height
        );
        const dataURL = canvas.toDataURL("image/jpeg", 1.0);
        app.ports.fromCropper.send(dataURL);
    };
});