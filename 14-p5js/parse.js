const input = `p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3`;

const data = input.split("\n").map((line) =>
  line.match(/p=(\d+),(\d+) v=(-?\d+),(-?\d+)/)
).map((m) => ({
  x: Number(m[1]),
  y: Number(m[2]),
  vx: Number(m[3]),
  vy: Number(m[4]),
}));
console.log(data);
