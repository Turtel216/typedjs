const x = true && false;
console.log(x);
console.log(true && true);
console.log(true && false);
console.log(false && false);
console.log(true || true);
console.log(true || false);
console.log(false || false);
console.log(!true);
console.log(!false);
if (!((true && false) || false)) {
  console.log(true);
}
