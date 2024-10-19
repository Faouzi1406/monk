# Explicit returns?

So how can we handle type inference of a function when a explicit return is found?

I have been thinking about this lately, and perhaps the best question is: Do we even want to allow for explicit returns?

I feel like we don't, since all typing is implicit. If our typesystem is implicit why would we allow for explict returning.

Also the inference would be kinda strange for this, how do we actually handle the return, do we somehow track and load it as a TyAnon into scope. Perhaps TyVar return?


# Params

```rust
let add(a, b) = {
// a and b are unused... Therefore we can not use them
    a + b
}

add(10, 20) -> a and b become type int

 a = b

|- add: 'a -> 'b -> 'c
_______________________
|- add: int -> int -> int
```

