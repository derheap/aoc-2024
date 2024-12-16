export const OPEN = 0;
export const TREE = 1;
export const LUMBER = 2;
export class World {
  constructor(sizeX, sizeY) {
    this.one = new Array(sizeX * sizeY);
    this.two = new Array(sizeX * sizeY);
    this.grid = this.one;
    this.phase = true;
    this.sizeX = sizeX;
    this.sizeY = sizeY;
  }

  get(x, y) {
    if (x >= 0 && x < this.sizeX && y >= 0 && y < this.sizeY) {
      return this.grid[y * this.sizeX + x];
    }
    return 0;
  }

  getType(x, y, type = 1) {
    if (x >= 0 && x < this.sizeX && y >= 0 && y < this.sizeY) {
      return this.grid[y * this.sizeX + x] == type ? 1 : 0;
    }
    return 0;
  }

  set(x, y, data) {
    if (x >= 0 && x < this.sizeX && y >= 0 && y < this.sizeY) {
      this.grid[y * this.sizeX + x] = data;
    }
  }

  setIn(array, x, y, data) {
    if (x >= 0 && x < this.sizeX && y >= 0 && y < this.sizeY) {
      array[y * this.sizeX + x] = data;
    }
  }

  initRandom(ptree = 0.5, plog = 0.2) {
    for (let x = 0; x < this.sizeX; x++) {
      for (let y = 0; y < this.sizeY; y++) {
        const r = Math.random();
        if (r <= ptree) {
          this.set(x, y, 1);
        } else if (r <= ptree + plog) {
          this.set(x, y, 2);
        } else {
          this.set(x, y, 0);
        }
      }
    }
  }

  blank() {
    for (let x = 0; x < this.sizeX; x++) {
      for (let y = 0; y < this.sizeY; y++) {
        this.set(x, y, 0);
      }
    }
  }

  splitForm = (form) => form.trim().split("\n").map((line) => line.split(""));
  setForm = (xx, yy, form) => {
    const parsed = this.splitForm(form);
    for (let y = 0; y < parsed.length; y++) {
      for (let x = 0; x < parsed[y].length; x++) {
        if (parsed[y][x] == "X") {
          this.set(xx + x, yy + y, 1);
        }
      }
    }
  };

  initWith(lines) {
    const data = lines.filter((it) => it.length > 0).map((line) =>
      line.split("")
    );
    for (let y = 0; y < data.length; y++) {
      for (let x = 0; x < data[y].length; x++) {
        if (data[y][x] == "|") {
          this.set(x, y, TREE);
        }
        if (data[y][x] == "#") {
          this.set(x, y, LUMBER);
        }
      }
    }
  }

  neighbours(x, y, type) {
    return this.getType(x - 1, y - 1, type) +
      this.getType(x + 0, y - 1, type) +
      this.getType(x + 1, y - 1, type) +
      this.getType(x - 1, y + 0, type) +
      //count += this.get(x + 0, y +0);// Ignore own cell
      this.getType(x + 1, y + 0, type) +
      this.getType(x - 1, y + 1, type) +
      this.getType(x + 0, y + 1, type) +
      this.getType(x + 1, y + 1, type);
  }

  neighbours1(x, y) {
    let count = 0;
    for (let xx = -1; xx <= 1; xx++) {
      for (let yy = -1; yy <= 1; yy++) {
        count += this.get(x + xx, y + yy);
      }
    }
    // Ignore own cell
    return count - this.get(x, y);
  }

  countLumber() {
    let count = 0;
    for (let y = 0; y < this.sizeY; y++) {
      for (let x = 0; x < this.sizeX; x++) {
        if (this.get(x, y) == LUMBER) {
          count += 1;
        }
      }
    }
    return count;
  }

  countTrees() {
    let count = 0;
    for (let y = 0; y < this.sizeY; y++) {
      for (let x = 0; x < this.sizeX; x++) {
        if (this.get(x, y) == TREE) {
          count += 1;
        }
      }
    }
    return count;
  }

  // Conways Game of Live
  // Rule B3/S23
  generate() {
    this.phase = !this.phase;

    let nextGen = this.phase ? this.one : this.two;
    for (let x = 0; x < this.sizeX; x++) {
      for (let y = 0; y < this.sizeY; y++) {
        const cell = this.get(x, y);
        if (cell == OPEN) {
          const trees = this.neighbours(x, y, TREE);
          if (trees >= 3) this.setIn(nextGen, x, y, TREE);
          else this.setIn(nextGen, x, y, OPEN);
        }
        if (cell == TREE) {
          const lumber = this.neighbours(x, y, LUMBER);
          if (lumber >= 3) this.setIn(nextGen, x, y, LUMBER);
          else this.setIn(nextGen, x, y, TREE);
        }
        if (cell == LUMBER) {
          const lumber = this.neighbours(x, y, LUMBER);
          const trees = this.neighbours(x, y, TREE);
          if (lumber >= 1 && trees >= 1) this.setIn(nextGen, x, y, LUMBER);
          else this.setIn(nextGen, x, y, OPEN);
        }
      }
    }
    this.grid = nextGen;
  }
}
