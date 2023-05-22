# Chapter 3 - Code

In this folder, I implement code from this chapter as it's used to implement the exercises. 

Regarding the `SymmetricList` type, I explore 6 different ways this type could be represented with the goal of making a performant type-safe representation. The only improvement that could still be made after `06-SymmetricList.purs` is to track how large each of the list's sizes are within the `Ends` constructor as that would remove the traversal we need to calculate the midpoint of each list.
