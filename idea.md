```rust
let check = 
    | Char '0'..'9' -> true
    | All -> false

let numeric = 
     | String s -> all check s 

let add = 
    | Int a -> Int b -> a + b
    | Float a -> Float b -> a + b
    | String a and (numeric a) -> String b and (numeric b) ->  make_int a +  b
```
