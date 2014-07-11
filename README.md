ADTFactory
==========

Proof of Concept for assisting in creating type-safe factories for a single sealed trait family (ADT) using Shapeless.
The ADT should be a single sealed trait or abstract class with case class members. 

A concrete factory should extend SealedFamilyFactory and provide a HList of constructor types for each ADT member. Note that the HList should be ordered alphabetically according to the type names of the members, and cannot provide any constuctor functions to types outside the ATD. Both limitations can be lifted but was not of interest for this PoC.
