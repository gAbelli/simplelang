# simplelang

A bytecode compiler and interpreter for a simple language (technically instructions are two bytes long but you get the idea).

## Example

```javascript
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
```

## Usage

Generate the parser using antlr4:

```bash
alias antlr4='java -jar /path/to/antlr-4.XX-complete.jar'
cd modules/parser/src/main/antlr4/
antlr4 -package parser -o ../java/parser/ SimpleLang.g4
cd -
```

Compile and run a program:

```bash
sbt 'vm/run <file>'
```
