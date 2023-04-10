# fried-stl

My own pastiche of the C++ STL.

The goal of this was to increase my understanding of how the STL is implemented, and to give me an excuse to write code with heavy template usage. The goal is very much *not* to have people use this, and in fact I plead with you to not do so. This project stays very close to what the standard prescribes, though in several places I make what I believe to be improvements.

Contained within the project is a partial implementation of the ranges and iterator libraries, many type traits and concepts, a tuple implementation that retains triviality and aggregate-ness, and many more miscellaneous things.

If I were to do this again today, I'd probably change several things, even if just how I handle implementation details. When I receive a compiler implementation with explicit object parameters, I may go at this again and redo most/all of it.
