```rust
let add(a, b) = a + b
let other = add(10, 20)

 ^ |-   + : int -> int -> int
 ----------------------------
 ^ |- a : int,^ |- b : int
 ----------------------------
 ^ |- add : int -> int -> int
 ----------------------------
 ^ |- other :  int
```
