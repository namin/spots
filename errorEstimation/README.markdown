Error Estimation
================

I translated the example _Using F# Quotations for Error Estimation_
from Chapter 9 of [Expert F#][1] (Introducing Language-Oriented
Programming) to Scala. I don't use anything as fancy as F#
quotations. Instead, I simply create a new arithmetic type for
estimates, and use implicit views to handily convert from the usual
arithmetic types.

[1]: http://www.expert-fsharp.com