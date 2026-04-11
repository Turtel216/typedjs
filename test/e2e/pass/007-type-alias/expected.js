const p = { first: 42, second: "test" };
console.log(p.first);
console.log(p.second);
const p1 = { first: 42, second: "test1" };
console.log(p1.first);
console.log(p1.second);
function getSecond(p) {
  return p.second;
}
console.log(getSecond(p));
console.log(getSecond(p1));
