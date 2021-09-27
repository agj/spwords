var app = Elm.Main.init({
  node: document.getElementById("elm"),
  flags: {
    viewport: {
      width: window.innerWidth,
      height: window.innerHeight,
    },
  },
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
