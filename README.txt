qua - I wanted to name it quanta, but qua is short and sweet.

Experimental entity-component-system.

Load with asdf or quicklisp or something.

Why entity component system?
From what I can gather the main reason for using it is to solve the problem of
code reusability in large variations of similar objects when using object
oriented inheritance. This problem seems to arise from the top down approach of
object definition in object orient programming. The way I see it, an object is
classified and given properties and behaviors that model the classification. In
contrast, entity component is a bottom up approach, where an object is given
properties and from that it automatically obtains behavior.
In essence it's just a means of simplifying variety. Google it for more info.

Structure of qua:
qua is loosely based off of Artemis in terms of api interface ideas.

* Entity
An entity is a unique positive integer.

* Component
A component is a group of data. In this case it's a class with data slots.

* System
A system handles entities which have certain specified components. UPDATE-SYSTEM
is the system' most important part. The macro WITH-COMPONENTS is the basic means
of using UPDATE-SYSTEM and allows for a quick way to handle all entities in the
system, writing the necessary code once. An example is in examples/examples.lisp .
INITIALIZE-SYSTEMS is useful for sorting all the entities into the correct
systems after all the components have been added to the entities.
SYSTEM-ADD-ENTITY is useful for directly putting an entity into a system, use
carefully.

* World
Everything is related to the world; it handles entities and systems.
Most of the important functions for handling entities and systems is in
world.lisp.

src :

* utils.lisp
Utility functions for qua.
Stuff a global world variable that might come in handy.
Functions to manipulate the hashtable of components of an entity.
Functions to get and set a component of a specified type of an entity.

* component.lisp
A component is just a class, a regular class definition will work with a system
if its slots have accessors with the same name as the slot.
The main interest in this file is the defcomponent macro which has parameters
NAME and SLOTS. NAME is the name of the class and SLOTS is the name of the
slots.

