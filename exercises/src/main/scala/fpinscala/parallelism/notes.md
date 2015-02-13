Exercise 7.1
------------

Signature of map2:

```scala
def map2(a: =>Par[A], b: =>Par[B])(f: (A, B) => C): Par[C]
```


Exercise 7.7
------------

Proof:
. map(map(y)(g))(f) == map(y)(f compose g)

Axioms:
1. Defn of map:
```scala
     def map[A,B](ap: Par[A])(f: A => B): Par[B] = {
       (es: ExecutorService) =>
         val a = ap(es).get()
         unit(f(a))
     }
```

2. Defn of compose
```scala
    def compose[A](f: T1 => R, g: A => T1): A => R = { x => f(g(x)) }
```

3. Map id
```scala
   map(y)(id) == y
```

4. Defn of unit
```scala
   def unit[A] = (es: ExecutorService) => UnitFuture(a)
```

Steps:
1. map(map(y)(g))(f)
Definition of map:
2. (es: ExecutorService) =>
      val a = map(y)(g)(es).get()
      unit(f(a))
Definition of map:
3. (es: ExecutorService) => {
      val a = (es2: ExecutorService) => {
           val b = y(es2).get()
           unit(g(b))
      }(es).get()
      unit(f(a))
   }
Expanding es2 == es
4. (es: ExecutorService) => {
    val a = unit(g(y(es)).get())(es).get()
    unit(f(a))
}
Expanding unit:
5. (es: ExecutorService) => {
    val a = g(y(es)).get()
    unit(f(a))
}
Subst a:
6. (es: ExecutorService) => {
    unit(f(g(y(es)))).get()
}
Defn of compose:
7. (es: ExecutorService) => {
    unit((f compose g)(y(es))).get()
}
Introduce tmp variable:
8. (es: ExecutorService) => {
    val a = y(es).get()
    unit((f compose g)(a)
}
Defn of map:
9: map(y)(f compose g)

