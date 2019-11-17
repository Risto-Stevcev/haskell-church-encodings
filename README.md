# Church Encodings in Haskell

This exercise was to demonstrate and play around with church encodings in a more involved way, and as a way for me to gain experience with haskell and it's type system.

**Church encodings** were developed by the late and famous Alonzo Church. Church is probably most well known for inventing **lambda calculus**, a formal branch of mathematics that introduces the notion of **lambdas**, or *anonymous functions*. You may have used them before when programming.

**Church encodings** are a very interesting development arising from **lambda calculus**. Church found out that every concept in programming languages can be represented using *functions*! everything from boolean logic, conditional statements, numbers (natural, integer, real, complex, imaginary), and even loops (infinite loops also)!

The most interesting thing about this is that numbers aren't anything special in math, they're just convenient placeholders. Math is really just logic in it's purest form.


## Rank1Types

My first attempt at fleshing out church encodings involved absolutely no type declarations, just to see how far I can go with Haskell's Hindley-Milner type system before I needed to introduce more complicated workings. I got as far as I could go without church subtraction. It turns out that the type system starts to choke just at that type because it's too polymorphic.


## RankNTypes

This is my final attempt and has worked for every church encoding I have attempted so far. I translated all of the encodings from the [church encoding](https://en.wikipedia.org/wiki/Church_encoding) wikipedia page. 

So far I've translated church booleans, church boolean operators, church conditionals, church comparison operators, church numerals (natural numbers), church arithmetic operators (for natural numbers), church lists, church 2-tuples, church integers, loops with the y-combinator, and church arithmetic operators (for integers) less the exponential and factorial operators. 

I can still translate the leftover operators for integers, as well as real, rational, imaginary and complex numbers.


## Credits

I'd like to give credit where it's due:

* [Martin's Blog](https://blogs.oracle.com/martink/entry/church_booleans_vs_javafx) for starting me off in the right direction.  

* [luqui](http://stackoverflow.com/questions/6595749/subtraction-of-church-numerals-in-haskell)'s StackOverflow post for uncovering how to move past the limitation's of Haskell's Hindley-Milner type system in order to implement church subtraction (and everything that follows it).

* The [church encoding](https://en.wikipedia.org/wiki/Church_encoding) wikipedia page for being so incredibly detailed and informative.

And finally, Church himself, for being such a badass!

![Alonzo Church](https://upload.wikimedia.org/wikipedia/en/a/a6/Alonzo_Church.jpg)  
Alonzo Church
