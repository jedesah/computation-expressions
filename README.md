# Expressions [![Build Status](https://api.shippable.com/projects/5542a8f8edd7f2c052dbbac3/badge?branchName=master)](https://app.shippable.com/projects/5542a8f8edd7f2c052dbbac3/builds/latest)

Expressions are a replacement for Scala for-comprehensions.

Scala for-comprehensions have three drawbacks that I have identified:

- They only use `flatMap` which can be unnecessarily restrictive
- They don't play very well with `if` and `match` statements
- It's not possible to manipulate the context from within a for-comprehension

The concepts behind Expressions are loosely related to Computation Expressions from F# which is the inspiration for the name.

Background on Computation Expressions:
- [http://tomasp.net/academic/papers/computation-zoo/computation-zoo.pdf](http://tomasp.net/academic/papers/computation-zoo/computation-zoo.pdf)
- [MSDN doc](https://msdn.microsoft.com/en-us/library/dd233182.aspx)

## Features

Note that the examples below do not explicit the type parameters of the `Expression` application. For some reason, these need to be explicit, I am still investigating why, so in reality, you need to do `Expression[Future, Int]{ combine(a,b,c) }`.


### Flexible use of abstractions

For-comprehensions are based on the notion of *do-notation* you may be familiar with from Haskell. This notation allows one to work with values within some kind of "context" which can be treated as a `Monad`. A `Monad` is any abstraction that supports the two following methods `point[A](a: A): F[A]` and `bind[A](fa: F[A])(f: A => F[B]): F[B]` (also know as `flatMap`). Both *do-notation* and for-comprehensions thus rewrite the "sugared" code into applications of `point` and `bind`. Unfortunately, while `bind` is a very powerful method and can be used to rewrite any expressions, it's power comes at the cost of flexibility in it's implementation. `Applicative` offers a less powerful abstraction that does however give more flexibility to it's implementation. What does this all mean, let's look at an example:

    a: Future[A]
    b: Future[B]
    c: Future[C]
    for {aa <- a
         bb <- b
         cc <- c} yield combine(a, b, c)

Let's assume `Future[A]` takes 5 seconds but `Future[C]` fails after 1 second. Let's assume we want it to "fail-fast", that is, we want the whole thing to fail immediately if `Future[C]` fails after 1 second. This behavior is simply impossible with the above code because `bind` does not support failing fast. Please see [my talk](https://www.youtube.com/watch?v=tU4pU5vaddU#t=823) at PNWScala for a more in depth explanation of why this is.

Now let's look at the same example using Expressions:

    a: Future[A]
    b: Future[B]
    c: Future[C]
    Expression { combine(a,b,c) }

This code will "fail-fast" because Expressions uses `ap` to combine these `Future`s which does support failing-fast

### Playing well with if statements and match statements

Let's consider the following piece of code:

    a: Future[A]
    b: Future[B]
    c: Future[C]
    for {aa <- a
         bb <- b
         cc <- c} yield if (aa == something) polish(bb) else polish(cc)

 The above code will wait on the result of all three `Future`'s before deciding to pick either the result from `b` or `c`. That's not ideal as far as I am concerned. I would rather if it waited on `a` and then waited on either `b` or `c` because at that point we know the result of the other one is of no importance.

 We can fix this like this:

     a: Future[A]
     b: Future[B]
     c: Future[C]
     (for (aa <- a) yield
       if (aa == something) for (bb <- b) yield polish(bb)
       else for (cc <- c) yield polish(cc)).flatMap(identity)

This code will do the right thing when it comes to discarding unnecessary `Future`s. In my book though, this is overly ceremonious.

Instead, we can do this:

    a: Future[A]
    b: Future[B]
    c: Future[C]
    Expression { if(extract(a) == something) polish(b) else polish(c) }
    
* Here is an example of somewhere automatic extraction is not triggered due to `==` being untyped. An explicit `extract` is required.

### Manipulating Context within an Expression (TODO)

Let's move away from `Future`s now because this notation supports any Context. Let's consider the `Writer` Monad. This Monad is typically used for logging.

for-comprehension version:

    a: Writer[Int] = Writer.point(8).log("This is a magic value")
    b: Writer[Int] = Writer.point(random()).log("I got this value from a dirty function")
    c: Writer[String] = Writer.point("Hello World").log("Hello World...")
    for {
      aa <- a
      bb <- b
      div <- if (b == 0) {
          Write.point(5).log("We avoided a division by zero, yay!")
        } else Write.point(aa / b)
      cc <- c
    } yield cc * div
    
Now with Expressions:

    a: Writer[Int] = Writer.point(8).log("This is a magic value")
    b: Writer[Int] = Writer.point(random()).log("I got this value from a dirty function")
    c: Writer[String] = Writer.point("Hello World").log("Hello World...")
    Expression {
      val div = if (b == 0) {
        ctx.log("We avoided a division by zero, yay!")
        5
      } else a / b
      c * div
    }

Conversely for `Future`s:

for comprehension:

    a: Future[Int] = Writer.point(8).log("This is a magic value")
    b: Future[Int] = Writer.point(random()).log("I got this value from a dirty function")
    c: Future[String] = Writer.point("Hello World").log("Hello World...")
    for {
      aa <- a
      bb <- b
      div <- if (b == 0) {
        Future.failed(new Exception("Division by zero :-("))
      } else Future.now(a / b)
      cc <- c
    } yield cc * div

Now with Expressions:

    a: Future[Int] = Writer.point(8).log("This is a magic value")
    b: Future[Int] = Writer.point(random()).log("I got this value from a dirty function")
    c: Future[String] = Writer.point("Hello World").log("Hello World...")
    Expression {
      val div = if (b == 0) {
        // return is a special function on the ctx that allows to change the whole Context to the value passed to it
        ctx.return(Future.failed(new Exception("Division by zero :-(")))
      } else a / b
      c * div
    }

## Getting Started

### Installation

    libraryDependencies += "com.github.jedesah" %% "expressions" % NOT-RELEASED-YET

Now, you can `import com.github.jedesah.Expression._` and use it like so:

    phoneString: String
    lookupPhone: String => Future[Phone]
    lookupAddress: Phone => Future[Address]
    lookupReputation: Phone => Future[Score]
    renderPage: (Phone, Address, Int) => HTML

    val response: Future[HTML] = Expression {
        val phone = extract(lookupPhone(phoneString))
        val address = extract(lookupAddress(phone))
        val rep = extract(lookupReputation(phone))
        renderPage(phone, address, rep)
    }

or if you prefer even more magic, like so:

    import com.github.Expression
    import Expression.auto.extract

    val response: Future[HTML] = Expression {
        val phone = lookupPhone(phoneString)
        val address = lookupAddress(phone)
        val rep = lookupReputation(phone)
        renderPage(phone, address, rep)
    }

## Limitations

Although the objective would be too support a maximum subset of the language, in practice we are far from there. Here are a bunch of constructs know to have good engouh support as well as some know to have some issues. If a construct is in neither category it's safe to assume it probably has some issues. The hope is that once Macro support improves in Scala ([Scala Meta)](http://scalameta.org/), it will be easier to support all of the language.

### Know to work well enough

- Function application
- if-else statement
- function currying
- String interpolation
- Blocks
- basic `match` statements
 
### Known limitations

- Does not support value pattern matching => `val (a,b) = something`
- The `match` statement is a challenge to get right in all cases. It should work fine for basic usage, but will start to break down if using stable identifiers and excessive monadic behavior in pattern matches. Value pattern matching is actually desagured to a pattern match which is why it is not supported.

## Similar Projects

### [Async/Await](https://github.com/scala/async)

*Async/Await* is based on a language feature from C#. It basically does the same thing, but specialized for `Future`'s. My experimentation reveals that it is not possible to "fail-fast". It might be better optimized for runtime performance.

### [Effectful](https://github.com/pelotom/effectful)

*Effectful* generalizes the idea behind Async/Await to any Monad. It currently does not use the least powerful interface and as such fundamentally does not support failing-fast for `Future`s.

### [Scala Workflow](https://github.com/aztek/scala-workflow)

Scala Workflow is the most advanced project out there doing a similar thing. It uses the least powerful interface and also allows injecting functions available on the abstraction right in the middle of a workflow. Unfortunately, Scala Workflow requires untyped macros which are no longer supported in the latest version of Macro Paradise. It also does more heavy lifting then I believe is necessary, for instance it implements the implicit extraction on it's own where as Expressions rely on good old Scala implicit resolution. This means that implicit extraction will break down in Expressions if the code has other unrelated implicit conversions (because Scala does not support multiple implicit conversions). IMHO this is a separate concern and I would rather see a macro or Scala itself support multiple implicit conversions with all the gotchas that come with it. So although Scala Workflow is in theory the most feature-full and generic solution, it's complicated implementation is weighting it down.

## TODO

- Fix all the limitations
- Implement `ctx`
- Support for nested abstractions
- Generalize tests
    - Currently the large majority of tests only test the `Option` context, there is no fundemental reason it could not be generalized to test many Contexts at the same time, although this would require some nifty changes to ScalaCheck or some alternative. See [how it's done in *Effectful*](https://github.com/pelotom/effectful/blob/master/src/test/scala/effectful/EffectfulSpec.scala#L18).
- Make ScalaCheck function generation better
    - Currenlty, ScalaCheck only generates constant functions, this means the properties run a much higher risk of being false positives. It would be possible to create a few function generators for our purposes, but much better would be to fix [this issue](https://github.com/rickynils/scalacheck/issues/136) on [ScalaCheck](https://github.com/rickynils/scalacheck).
- Use [Scala Meta](http://scalameta.org/)
