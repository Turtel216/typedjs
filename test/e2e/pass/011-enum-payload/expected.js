const s = { _tag: "Circle", _0: 5 };
const area = (() => {
  const _m = s;
  if (_m._tag === "Circle") {
    const r = _m._0;
    return r * r * 3;
  }
  if (_m._tag === "Rect") {
    const w = _m._0;
    const h = _m._1;
    return w * h;
  }
  if (_m._tag === "Point") {
    return 0;
  }
})();
console.log(area);
const s2 = { _tag: "Rect", _0: 4, _1: 6 };
const area2 = (() => {
  const _m = s2;
  if (_m._tag === "Circle") {
    const r = _m._0;
    return r * r * 3;
  }
  if (_m._tag === "Rect") {
    const w = _m._0;
    const h = _m._1;
    return w * h;
  }
  if (_m._tag === "Point") {
    return 0;
  }
})();
console.log(area2);
