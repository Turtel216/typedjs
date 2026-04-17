const x = { _tag: "Some", _0: 42 };
const val = (() => {
  const _m = x;
  if (_m._tag === "Some") {
    const v = _m._0;
    return v + 1;
  }
  if (_m._tag === "None") {
    return 0;
  }
})();
console.log(val);
const y = { _tag: "None" };
const msg = (() => {
  const _m = y;
  if (_m._tag === "Some") {
    const s = _m._0;
    return s;
  }
  if (_m._tag === "None") {
    return "empty";
  }
})();
console.log(msg);
