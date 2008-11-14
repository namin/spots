Probabilistic Modeling
======================

For my induction into Scala, I wanted to translate the probabilistic monad of Chapter 9 of [Expert F#][1] (Introducing Language-Oriented Programming). The idea, based on the paper *[Stochastic Lambda Calculus and Monads of Probability Distributions][2]*, is to define a probability monad to compute over distributions of a domain instead of the domain itself. We limit ourselves to distributions over discrete domains characterized by three functions: 

1. sampling
2. support 
   (i.e. a set of values where all elements outside the set have zero chance of being sampled)
3. expectation of a function over the distribution 
   (e.g. the probability of selecting element `A` by evaluating the function `f(x) = 1` if `x` equals `A` and `0` otherwise)

[1]: http://www.expert-fsharp.com
[2]: http://www.cs.tufts.edu/~nr/pubs/pmonad-abstract.html