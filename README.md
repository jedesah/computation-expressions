# Idiom Brackets
An implementation of Idiom Brackets in Scala in the form of a macro definition.

## Features
- Uses the least powerful abstraction required to desugar the code within the bracket (this means that it can fail-fast with `Future`s)
- Syntactic sugar for any abstraction that has instances for Scalaz Apply, Applicative, Monad
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

Idiom brackets generalizes the concept behind Async/Await that you can both find in C# and Scala. With Idiom Brackets you can use the same notation as an alternative to any `Applicative` or `Monad`, ie. `Future`, `Option`, `List`, `\/`, `Writer`, etc. To top it off, Scala/Async does not fail-fast when a Future has failed, it just sort of waits on the first one. With Idiom Brackets, it's easy to supply and instance of `Applicative` and because it uses the least powerful abstraction require to accomplish the desaguring, your code will fail "as fast as logically possible". If you would prefer not to fail-fast and keep the behavior of Async/Await, you can supply an instance of `Applicative` that does that too. Basically, thanks to the use of scalaz `Applicative`, one can easily configure the minutia of how your abstractions are combined.

### What about [Effectful](https://github.com/pelotom/effectful)

I found out about Effectful after been well underway in my own project. It currently does not use the least powerful interface and as such fundamentally does not support failing-fast `Future`s which was one of my main design goals. It would most probably be possible to add this to Effectful, but I decided to move forward with my own project because I was comfortable with the code base and it did things.

### What about [Scala Workflow](https://github.com/aztek/scala-workflow)

Scala Workflow is the most advanced project out there doing a similar thing. It uses the least powerful interface and also allows injecting functions available on the abstraction right in the middle of a Bracket. Unfortunatly, Scala Workflow requieres Untyped macros which are no longer supported. It also does more heavy lifting then I believe necessary, for instance it implements the implicit extraction on it's own where as Idiom Brackets rely on good old Scala implicit resolution. This means that implicit extraction will break down in Idiom Brackets if the code has other unrelated implicit conversions (because Scala does not support multiple implicit conversions). IMHO this is a seperate concern and I would rather see a macro or Scala itself support multiple implicit conversions with all the gotchas that implies. So although Scala Workflow is in theory the most featureful and generic solution, it's complicated implementation is weighting it down.

## Instructions

I will hopefully soon post instructions on how to get and use Idiom Brackets. It's still very much experimentatl. Feel free to submit feedback!
