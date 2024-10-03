function fib(n) {
  let a = 0;
  let b = 1;
  while (n > 0) {
    let tmp = b;
    b = a + b;
    a = tmp;
    n = n - 1;
  }
  return a;
}

function fibSlow(n) {
  if (n == 0) {
    return 0;
  }
  if (n == 1) {
    return 1;
  }
  return fibSlow(n - 1) + fibSlow(n - 2);
}

function main() {
  let n = 20;
  let _ = print(fibSlow(n));
  let _ = print(fib(n));
}
