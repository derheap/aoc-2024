let ui = { form: {}, redraw: true, start: false };

const tap = (data) => {
  console.log("TAP %O", data);
  return data;
};

const sizeX = 101;
const sizeY = 103;
let dX;
let dY;
let open;
let input;
let gen = 0;
let robots = [];
let part = 2;
let smallestAt = 0;
let smallest = sizeX * sizeY + 1;
let quads;
let rPerQuad;

export function preload() {
  input = loadStrings("./14.txt");
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
  createCanvas(600, 600);
  dX = width / sizeX;
  dY = height / sizeY;

  colorMode(HSL);
  open = color(51, 80, 50);
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
  //frameRate(30);
  quads = calcQuads(5);
  console.log(quads);
  rPerQuad = robots.length / (5 * 5);
  console.log(rPerQuad);
}

const moveARobot = (r) => {
  r.x = r.x + r.vx;
  r.y = r.y + r.vy;
  //r.x = r.x >= 0 ? r.x % sizeX : (r.x + sizeX) % sizeX;
  //r.y = r.y >= 0 ? r.y % sizeY : (r.y + sizeY) % sizeY;
  if (r.x < 0) {
    r.x += sizeX;
  }
  if (r.x >= sizeX) {
    r.x = r.x - sizeX;
  }
  if (r.y < 0) {
    r.y += sizeY;
  }
  if (r.y >= sizeY) {
    r.y = r.y - sizeY;
  }
  if (r.x < 0 || r.y < 0 || r.x >= sizeX || r.y >= sizeY) {
    console.log(gen, r);
  }
  //if (r.x == 0 && r.y == 0) console.log(gen, r);
};

const drawRobots = () => {
  background(open);
  stroke(64);
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
};

const run = (frames) => {
  while (frames--) {
    if (gen % 100 == 0) {
      drawRobots();
    }
    const resultP1 = saftyFactor(robots);
    if (gen == 100) {
      ui.output.innerText = "p1: " + resultP1;
      if (resultP1 != 226548000) {
        noLoop();
      }
      drawRobots();
    }
    const d = density(robots, quads, 1);
    //console.log(d.length);
    const area = calcArea(robots);
    if (area < smallest || d.length) {
      smallest = area;
      smallestAt = gen;
      //console.log(smallest, "@", smallestAt);
      drawRobots();
      saveCanvas("robots-" + gen, "png");
    }
    robots.map(moveARobot);
    gen += 1;
  }
};

export function draw() {
  if (ui.redraw || ui.start) {
    ui.redraw = false;
    ui.gen.innerText = gen;
    run(1000);
  }
}

export function keyTyped() {
  console.log(key);
  if (key == "r") ui.redraw = true;
}

const saftyFactor = (robots) => {
  const quadrants = [{
    xmin: 0,
    ymin: 0,
    xmax: Math.floor(sizeX / 2) - 1,
    ymax: Math.floor(sizeY / 2) - 1,
  }, {
    xmin: Math.floor(sizeX / 2) + 1,
    ymin: 0,
    xmax: sizeX - 1,
    ymax: Math.floor(sizeY / 2) - 1,
  }, {
    xmin: 0,
    ymin: Math.floor(sizeY / 2) + 1,
    xmax: Math.floor(sizeX / 2) - 1,
    ymax: sizeY - 1,
  }, {
    xmin: Math.floor(sizeX / 2) + 1,
    ymin: Math.floor(sizeY / 2) + 1,
    xmax: sizeX - 1,
    ymax: sizeY - 1,
  }];
  const count = quadrants.map((q) =>
    robots.filter(
      (r) => r.x >= q.xmin && r.x <= q.xmax && r.y >= q.ymin && r.y <= q.ymax,
    )
  );
  return count.reduce((acc, value) => acc * value.length, 1);
};

const calcQuads = (parts) => {
  const dx = Math.floor(sizeX / parts);
  const dy = Math.floor(sizeY / parts);
  let quads = [];
  for (let x = 0; x < parts; x++) {
    for (let y = 0; y < parts; y++) {
      quads.push({
        xmin: x * dx,
        ymin: y * dy,
        xmax: (x + 1) * dx,
        ymax: (y + 1) * dy,
      });
    }
  }
  return quads;
};

const density = (robots, quads, limit) =>
  quads.map((q) =>
    robots.filter(
      (r) => r.x >= q.xmin && r.x <= q.xmax && r.y >= q.ymin && r.y <= q.ymax,
    ).length
  ).filter((c) => c < limit);

const calcArea = (robots) => {
  let xmin = sizeX;
  let xmax = -1;
  let ymin = sizeY;
  let ymax = -1;
  robots.forEach((r) => {
    if (r.x < xmin) xmin = r.x;
    if (r.x > xmax) xmax = r.x;
    if (r.y < ymin) ymin = r.y;
    if (r.y > ymax) ymax = r.y;
  });
  return (xmax - xmin + 1) * (ymax - ymin + 1);
};
