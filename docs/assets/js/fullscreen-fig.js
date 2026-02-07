is_fullscreen = function () {
  return Boolean(
    document.fullscreenElement ||
    document.webkitFullscreenElement ||
    document.mozFullScreenElement ||
    document.msFullscreenElement,
  );
};

toggle_fs_class = function (el) {
  if (Array.from(el.classList).includes("fig-fs")) {
    el.classList.remove("fig-fs");
  } else {
    el.classList.add("fig-fs");
  }
};

add_fs_events = function (el) {
  el.addEventListener("click", () => {
    if (is_fullscreen()) {
      document.exitFullscreen();
    } else {
      el.requestFullscreen();
    }
  });
  el.addEventListener("fullscreenchange", () => {
    toggle_fs_class(el);
  });
};

window.addEventListener("load", () => {
  document.querySelectorAll("div.quarto-figure").forEach((el) => {
    add_fs_events(el);
  });
});
