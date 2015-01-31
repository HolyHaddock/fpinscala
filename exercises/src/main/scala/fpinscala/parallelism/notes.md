Exercise 7.1
------------

Signature of map2:

```scala
def map2(a: =>Par[A], b: =>Par[B])(f: (A, B) => C): Par[C]
```