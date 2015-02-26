# Computation Expressions
An implementation of Computation Expressions in Scala in the form of a macro definition.

Background on Computation Expressions:
- [http://tomasp.net/academic/papers/computation-zoo/computation-zoo.pdf](http://tomasp.net/academic/papers/computation-zoo/computation-zoo.pdf) which describes very closely what I am trying to accomplish here. Which is why I changed names from Idiom Brackets which in the literature are a notation for Applicative Functors exclusively.
- [MSDN doc](https://msdn.microsoft.com/en-us/library/dd233182.aspx)

## Features
- Uses the least powerful abstraction required to desugar the code within the bracket (this means, among other things, that it can fail-fast with `Future`s)
- Syntactic sugar for any abstraction that has instances for Scalaz Apply, Applicative or Monad
- The extraction can happen implicitly or explicitly

## Example

    phoneString: String
    lookupPhone: String => Future[Phone]
    lookupAddress: Phone => Future[Address]
    lookupReputation: Phone => Future[Score]
    renderPage: (Phone, Address, Int) => HTML

    val response: Future[HTML] = IdiomBracket.monad {
        val phone = lookupPhone(phoneString)
        val address = lookupAddress(phone)
        val rep = lookupReputation(phone)
        renderPage(phone, address, rep)
    }
    
Note: With the appropriate instance for `Monad[Future]`, the response Future will fail as soon as any of `lookupPhone`, `lookupAddress` or `lookupReputation` fails. It "detects" that no response is possible if any of these fails.
    
## Prior art

### Isn't this the same thing as [Async/Await](https://github.com/scala/async)

Do-notation generalizes the concept behind Async/Await that you can both find in C# and Scala. Do-notation can be used with any `Monad` such as `Future`, `Option`, `List`, `\/`, `Writer`, etc. Computation Expressions generalize do-notation, it not only supports the `Monad` abstraction, but also `Applicative` (Idiom Brackets) and `Apply` (no name for a notation specific to this abstraction that I am aware of). It uses the least powerful interface required which in turn means more flexibility. For instance, it is possible to supply a `Monad` instance for `Future`s that fails-fast if the notation used does not simply always use `bind`. `bind` does not permit failing fast because of it's signature. See my talk at [PNWScala](https://www.youtube.com/watch?v=tU4pU5vaddU#t=823) for an in depth explanation about why that is. I am not sure how Scala/Async is implemented, but I have observed that it does not fail-fast when a Future has failed, it just sort of waits on the first one.

### What about [Effectful](https://github.com/pelotom/effectful)

I found out about Effectful after been well underway in my own project. It currently does not use the least powerful interface and as such fundamentally does not support failing-fast `Future`s which was one of my main design goals. It would most probably be possible to add this to Effectful, but I decided to move forward with my own project because I was comfortable with the code base and it did things. In it's current state, it's a generalization of Scala/Async to any other Monad, or a slightly more convenient for-comprehension syntax.

### What about [Scala Workflow](https://github.com/aztek/scala-workflow)

Scala Workflow is the most advanced project out there doing a similar thing. It uses the least powerful interface and also allows injecting functions available on the abstraction right in the middle of a workflow. Unfortunatly, Scala Workflow requieres Untyped macros which are no longer supported in the latest version of Scala. It also does more heavy lifting then I believe necessary, for instance it implements the implicit extraction on it's own where as Idiom Brackets rely on good old Scala implicit resolution. This means that implicit extraction will break down in Idiom Brackets if the code has other unrelated implicit conversions (because Scala does not support multiple implicit conversions). IMHO this is a seperate concern and I would rather see a macro or Scala itself support multiple implicit conversions with all the gotchas that implies. So although Scala Workflow is in theory the most featureful and generic solution, it's complicated implementation is weighting it down.

## Instructions

I will hopefully soon post instructions on how to get and use Computation Expressions. It's still very much experimentatl. Feel free to submit feedback!
