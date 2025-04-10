// A minimal System 6-style window system rendered to HTML5 canvas
const canvas = document.getElementById("screen");
const ctx = canvas.getContext("2d");
canvas.width = 640;
canvas.height = 480;

ctx.imageSmoothingEnabled = false;
ctx.imageSmoothingEnabled = false;
ctx.webkitImageSmoothingEnabled = false;
ctx.mozImageSmoothingEnabled = false;
ctx.msImageSmoothingEnabled = false;

// Simple window representation
class Sys6Window {
  constructor(x, y, w, h, title, modal = false) {
    this.x = x;
    this.y = y;
    this.width = w;
    this.height = h;
    this.title = title;
    this.dragging = false;
    this.resizing = false;
    this.dragOffsetX = 0;
    this.dragOffsetY = 0;
    this.modal = modal;
    this.hasScroll = true;
    this.scrollY = 0;
  }

  draw(ctx) {
    if (this.modal) {
      ctx.fillStyle = "rgba(0, 0, 0, 0.25)";
      ctx.fillRect(0, 0, canvas.width, canvas.height);
    }

    ctx.fillStyle = "#ffffff";
    ctx.fillRect(this.x, this.y + 16, this.width, this.height - 16);
    ctx.strokeStyle = "#000000";
    ctx.strokeRect(this.x, this.y, this.width, this.height);

    ctx.fillStyle = "#c0c0c0";
    ctx.fillRect(this.x, this.y, this.width, 16);
    ctx.strokeRect(this.x, this.y, this.width, 16);

    ctx.fillStyle = "#000000";
    ctx.fillRect(this.x + 2, this.y + 2, 10, 10);

    ctx.font = "12px monospace";
    ctx.fillStyle = "#000000";
    ctx.fillText(this.title, this.x + 16, this.y + 12);

    if (this.hasScroll) {
      ctx.fillStyle = "#e0e0e0";
      ctx.fillRect(this.x + this.width - 12, this.y + 16, 12, this.height - 16);
      ctx.fillStyle = "#808080";
      const knobHeight = 30;
      const knobY = this.y + 16 + this.scrollY;
      ctx.fillRect(this.x + this.width - 12, knobY, 12, knobHeight);
    }

    ctx.strokeStyle = "#000000";
    ctx.beginPath();
    ctx.moveTo(this.x + this.width - 1, this.y + this.height - 10);
    ctx.lineTo(this.x + this.width - 10, this.y + this.height - 1);
    ctx.stroke();

    // Draw modal dismiss button
    if (this.modal) {
      ctx.fillStyle = "#a0a0a0";
      ctx.fillRect(this.x + this.width / 2 - 30, this.y + this.height - 30, 60, 20);
      ctx.strokeRect(this.x + this.width / 2 - 30, this.y + this.height - 30, 60, 20);
      ctx.fillStyle = "#000000";
      ctx.font = "12px monospace";
      ctx.fillText("OK", this.x + this.width / 2 - 10, this.y + this.height - 15);
    }
  }

  isInTitleBar(x, y) {
    return x >= this.x && x <= this.x + this.width && y >= this.y && y <= this.y + 16;
  }

  isInCloseBox(x, y) {
    return x >= this.x + 2 && x <= this.x + 12 && y >= this.y + 2 && y <= this.y + 12;
  }

  isInScrollBar(x, y) {
    return x >= this.x + this.width - 12 && x <= this.x + this.width && y >= this.y + 16 && y <= this.y + this.height;
  }

  isInResizeHandle(x, y) {
    return x >= this.x + this.width - 12 && y >= this.y + this.height - 12;
  }

  isInDismissButton(x, y) {
    return this.modal && x >= this.x + this.width / 2 - 30 && x <= this.x + this.width / 2 + 30 && y >= this.y + this.height - 30 && y <= this.y + this.height - 10;
  }
}

const windows = [];

function drawAll() {
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  for (const win of windows) {
    win.draw(ctx);
  }
}

drawAll();

function animateZoom(x, y, w, h, zoomIn, callback) {
  const steps = 8;
  let current = 0;
  const origin = { x: canvas.width / 2, y: canvas.height / 2 };
  const anim = () => {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    drawAll();
    ctx.save();
    ctx.setLineDash([4, 2]);
    ctx.strokeStyle = "#000000";
    const progress = zoomIn
      ? current / steps
      : 1 - current / steps;
    const drawW = w * progress;
    const drawH = h * progress;
    const drawX = origin.x - drawW / 2;
    const drawY = origin.y - drawH / 2;
    ctx.strokeRect(drawX, drawY, drawW, drawH);
    ctx.restore();
    current++;
    if (current <= steps) {
      requestAnimationFrame(anim);
    } else {
      callback();
    }
  };
  anim();
}

let activeWindow = null;
let draggingScroll = false;
let scrollOffsetY = 0;
let resizingWindow = false;
let outlineStart = null;

function drawOutline(x, y, w, h) {
  ctx.save();
  ctx.setLineDash([4, 2]);
  ctx.strokeStyle = "#000000";
  ctx.strokeRect(x, y, w, h);
  ctx.restore();
}

function openWindow(win) {
  animateZoom(win.x, win.y, win.width, win.height, true, () => {
    windows.push(win);
    drawAll();
  });
}

canvas.addEventListener("mousedown", (e) => {
  const rect = canvas.getBoundingClientRect();
  const mouseX = e.clientX - rect.left;
  const mouseY = e.clientY - rect.top;

  for (let i = windows.length - 1; i >= 0; i--) {
    const win = windows[i];
    if (win.modal && win.isInDismissButton(mouseX, mouseY)) {
      animateZoom(win.x, win.y, win.width, win.height, false, () => {
        windows.splice(i, 1);
        drawAll();
      });
      return;
    }
    if (win.isInResizeHandle(mouseX, mouseY)) {
      resizingWindow = true;
      activeWindow = win;
      outlineStart = { x: win.x, y: win.y, w: win.width, h: win.height };
      return;
    }
    if (win.isInTitleBar(mouseX, mouseY)) {
      if (win.isInCloseBox(mouseX, mouseY)) {
        if (!win.modal) {
          const closingWin = windows[i];
          animateZoom(closingWin.x, closingWin.y, closingWin.width, closingWin.height, false, () => {
            windows.splice(i, 1);
            drawAll();
          });
        }
        return;
      }
      if (!win.modal) {
        win.dragging = true;
        win.dragOffsetX = mouseX - win.x;
        win.dragOffsetY = mouseY - win.y;
        activeWindow = win;
        outlineStart = { x: win.x, y: win.y, w: win.width, h: win.height };
      }
      return;
    } else if (win.isInScrollBar(mouseX, mouseY)) {
      draggingScroll = true;
      scrollOffsetY = mouseY - win.scrollY;
      activeWindow = win;
      return;
    }
  }
});

canvas.addEventListener("mousemove", (e) => {
  const rect = canvas.getBoundingClientRect();
  const mouseX = e.clientX - rect.left;
  const mouseY = e.clientY - rect.top;

  drawAll();

  if (activeWindow && activeWindow.dragging) {
    const newX = mouseX - activeWindow.dragOffsetX;
    const newY = mouseY - activeWindow.dragOffsetY;
    drawOutline(newX, newY, activeWindow.width, activeWindow.height);
  } else if (activeWindow && draggingScroll) {
    activeWindow.scrollY = Math.min(Math.max(0, mouseY - scrollOffsetY), activeWindow.height - 46);
    drawAll();
  } else if (resizingWindow && activeWindow) {
    const newW = Math.max(100, mouseX - activeWindow.x);
    const newH = Math.max(50, mouseY - activeWindow.y);
    drawOutline(activeWindow.x, activeWindow.y, newW, newH);
  }
});

canvas.addEventListener("mouseup", (e) => {
  const rect = canvas.getBoundingClientRect();
  const mouseX = e.clientX - rect.left;
  const mouseY = e.clientY - rect.top;

  if (activeWindow && activeWindow.dragging) {
    activeWindow.x = mouseX - activeWindow.dragOffsetX;
    activeWindow.y = mouseY - activeWindow.dragOffsetY;
    activeWindow.dragging = false;
  }

  if (resizingWindow && activeWindow) {
    activeWindow.width = Math.max(100, mouseX - activeWindow.x);
    activeWindow.height = Math.max(50, mouseY - activeWindow.y);
  }

  activeWindow = null;
  draggingScroll = false;
  resizingWindow = false;
  outlineStart = null;
  drawAll();
});

// Example windows
openWindow(new Sys6Window(100, 100, 200, 150, "Window 1"));
openWindow(new Sys6Window(150, 150, 200, 150, "Window 2"));
openWindow(new Sys6Window(180, 180, 180, 100, "Modal Alert", true));
