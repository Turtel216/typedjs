if (1 < 2) {
  console.log(true);
}
if (1 > 2) {
  console.log(false);
}
if (1 < 2) {
  console.log(true);
} else {
  console.log(false);
}
if (1 > 2) {
  console.log(false);
} else {
  console.log(true);
}
let sum = 0;
while (sum < 10) {
  sum = sum + 1;
}
console.log(sum);
const x = 1 < 2 ? true : false;
console.log(x);
const x1 = 1 > 2 ? false : true;
console.log(x1);
const x2 = 1 < 2 ? (2 + 2 * 2) : (2 - 2 * 2);
console.log(x2);
const x3 = 1 < 2 ? true : false;
console.log(x3);
const x4 = 1 < 2 ? (1 != 1 ? false : true) : false;
console.log(x4);
