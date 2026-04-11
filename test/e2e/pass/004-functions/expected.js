function id(n) {
  return n;
}
function inc(n) {
  return n + 1;
}
function inc1(n) {
  return n + 1;
}
function add(a, b) {
  return a + b;
}
function add1(obj) {
  return obj.a + obj.b;
}
function objReturn(x, y) {
  return { a: x, b: y };
}
const x = id(1);
const obj = { a: 1, b: 1 };
const obj1 = objReturn(1, 1);
console.log(id(1));
console.log(id(true));
console.log(inc(1));
console.log(inc1(1));
console.log(add(1, 1));
console.log(add1(obj));
console.log(obj1.a + obj1.b);
function apply(a, f) {
  return f(a);
}
console.log(apply(1, id));
function apply1(a, f) {
  return f(a);
}
console.log(apply1(1, id));
console.log(apply1(true, id));
