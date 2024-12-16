let ui = { form: {}, redraw: true, start: false };

const tap = (data) => {
  console.log("TAP %O", data);
  return data;
};

const sizeX = 11;
const sizeY = 7;
let dX;
let dY;
let output;
let world;
let lumber;
let tree;
let open;
let input;
let gen = 0;
let worlds = [];
let found = false;
let goal = 1000000000;
let robots = [];

export function preload() {
  input = loadStrings("./14-test.txt");
}

const parse = (text) =>
  text.split("\n").map((line) => line.match(/p=(\d+),(\d+) v=(-?\d+),(-?\d+)/))
    .map((m) => ({
      x: Number(m[1]),
      y: Number(m[2]),
      vx: Number(m[3]),
      vy: Number(m[4]),
    }));

export function setup() {
  robots = parse(input.join("\n").trim());
  console.log(robots);
  createCanvas(800, 800);
  dX = width / sizeX;
  dY = height / sizeY;

  colorMode(HSL);
  open = color(51, 80, 50);
  lumber = color(33, 100, 25);
  tree = color(116, 80, 25);
  ui.output = document.querySelector("#log");
  ui.gen = document.querySelector("#gen");
  ui.form.redraw = document.querySelector("#ui-redraw");
  ui.form.redraw.addEventListener("click", (e) => {
    ui.redraw = true;
    e.preventDefault();
  });
  ui.form.start = document.querySelector("#ui-start");
  ui.form.start.addEventListener("click", (e) => {
    ui.start = true;
    e.preventDefault();
  });
}

const moveARobot = (r) => {
  //console.log("0", r);
  r.x += r.vx;
  r.y += r.vy;
  //console.log("1", r);
  r.x = r.x >= 0 ? r.x % sizeX : (r.x + sizeX) % sizeX;
  r.y = r.y >= 0 ? r.y % sizeY : (r.y + sizeY) % sizeY;
  //console.log("2", r);
};

export function draw() {
  if (ui.redraw || ui.start) {
    ui.redraw = false;
    background(open);
    stroke(64);
    ui.gen.innerText = gen;
    for (let x = 0; x < sizeX; x++) {
      for (let y = 0; y < sizeY; y++) {
        fill(open);
        rect(x * dX, y * dY, dX, dY);
      }
    }
    robots.forEach((it) => {
      fill(color(180 + it.vx * 10, 66 + it.vy * 5, 40));
      rect(it.x * dX, it.y * dY, dX, dY);
    });
    robots.map(moveARobot);
    gen += 1;
  }
}

export function keyTyped() {
  console.log(key);
  if (key == "r") ui.redraw = true;
}
