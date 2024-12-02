import { assertEquals } from "jsr:@std/assert";

const decoder = new TextDecoder("utf-8");
const input = decoder.decode(Deno.readFileSync("input/01.txt")).trim();

const test_input = `3   4
4   3
2   5
1   3
3   9
3   3`;

const splitLines = (text) => text.split("\n");
const splitLine = (line) => line.split(/\s+/);
const toNumbers = (input) =>
  splitLines(input).map(splitLine).map((list) => list.map(Number));
const sum = (accu, value) => accu + value;

const tap = (msg, data) => {
  console.log(msg, data);
  return data;
};

const makeList = (list, idx) => list.map((item) => item[idx]);

const part_1 = (input) => {
  const numbers = toNumbers(input);
  const left = makeList(numbers, 0).sort();
  const right = makeList(numbers, 1).sort();
  return left.map((it, idx) => Math.abs(it - right[idx])).reduce(sum);
};

assertEquals(part_1(test_input), 11);
console.log("Part 1:", part_1(input));

const part_2 = (input) => {
  const numbers = toNumbers(input);
  const left = makeList(numbers, 0).sort();
  const right = makeList(numbers, 1).sort();
  return left.map((num, idx) => num * right.filter((it) => num == it).length)
    .reduce(
      sum,
    );
};

assertEquals(part_2(test_input), 31);
console.log("Part 2:", part_2(input));
