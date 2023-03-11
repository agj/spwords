import { Elm } from '../elm/Main.elm'

const saveStateKey = "spwords-state";

var app = Elm.Main.init({
  node: document.getElementById("elm"),
  flags: {
    viewport: {
      width: window.innerWidth,
      height: window.innerHeight,
    },
    saveState: localStorage.getItem(saveStateKey),
  },
});

app.ports.command.subscribe(({ kind, value }) => {
  switch (kind) {
    case "saveState":
      localStorage.setItem(saveStateKey, value);
      break;
  }
});

if (visualViewport) {
  visualViewport.addEventListener("resize", (e) => {
    window.scrollTo(0, 0);
  });

  document.addEventListener("scroll", (e) => {
    window.scrollTo(0, 0);
  });

  document.body.addEventListener(
    "touchmove",
    (e) => {
      e.preventDefault();
    },
    { passive: false }
  );
}
