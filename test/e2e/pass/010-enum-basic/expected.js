const c = { _tag: "Red" };
const name = (() => {
  const _m = c;
  if (_m._tag === "Red") {
    return "red";
  }
  if (_m._tag === "Green") {
    return "green";
  }
  if (_m._tag === "Blue") {
    return "blue";
  }
})();
console.log(name);
const c2 = { _tag: "Blue" };
const name2 = (() => {
  const _m = c2;
  if (_m._tag === "Red") {
    return "r";
  }
  return "not red";
})();
console.log(name2);
