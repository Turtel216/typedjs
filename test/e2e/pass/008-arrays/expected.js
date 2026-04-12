const arr = [1, 2, 3, 4];
const arr1 = [1, 2, 3, 4];
let i = 0;
while (i < 4) {
  console.log(arr[i]);
  i = i + 1;
}
let arr2 = [1, 1, 1];
arr2[1] = arr2[1] + 1;
console.log(arr2[1]);
const arr3 = [true, true];
console.log(arr3[0]);
function getIndex(arr, i) {
  return arr[i];
}
console.log(getIndex(arr, 3));
function getIndex1(arr, i) {
  return arr[i];
}
console.log(getIndex1(arr, 3));
