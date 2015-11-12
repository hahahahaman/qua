## qua - Experimental entity-component-system.

### Installation

  Load with asdf or quicklisp or something.

### Why entity component system?

  From what I can gather the main reason for using it is to solve the problem of
code reusability in large variations of similar objects when using object
oriented inheritance. This problem seems to arise from the top down approach of
object definition in object orient programming. The way I see it, an object is
classified and given properties and behaviors that model the classification, in
contrast, entity component is a bottom up approach, where an object is given
properties and from that it automatically obtains behavior. Essentially it's a
means of simplifying variety. Google it for more info.

### Structure of qua:

  qua is loosely based off of [Artemis](https://github.com/junkdog/artemis-odb) in
terms of api ideas.

* Entity

  An entity is a unique positive integer, which I'll refer to as an entity-id.

* Component

  A component is a group of data. In this case it's a class or a struct with data
slots.

* System

  A system handles entities which have certain specified components.
UPDATE-SYSTEM is the system's most important part. The macro WITH-COMPONENTS is
the basic means of using UPDATE-SYSTEM and allows for a quick way to handle all
entities in the system, writing the necessary code once. An example is in
examples/examples.lisp . INITIALIZE-SYSTEMS is useful for sorting all the
entities into the correct systems after all the components have been added to
the entities. SYSTEM-ADD-ENTITY is useful for directly putting an entity into a
system, use carefully.

* World

  Everything is related to the world; it handles entities and systems.
Most of the important functions for handling entities and systems are in
world.lisp.

#### src:

* utils.lisp

  Utility functions for qua. Contains a global world variable that might come in
handy. Functions to manipulate the hashtable of components of an entity.
Functions to get and set a component of a specified type of an entity.

* component.lisp

  A component is just a class (a struct can also be used, since it doesn't
actually use any CLOS specific features). The main interest in this file is the
DEFCOMPONENT macro, which has parameters NAME and SLOTS. NAME is the name of the
class and SLOTS is the name of the slots.

* system.lisp

  A system is a class with two slots: DEPENDENCIES and ENTITIES. DEPENDENCIES is
a list of type symbols of the necessary components. ENTITIES is a hashtable with
keys of the entity id and a value of 1, which is a place holder.

* world.lisp

  The class has 3 slots: ENTITY-COMPONENTS, ENTITY-IDS, and SYSTEMS.

    * ENTITY-COMPONENTS is a hashtable with keys of the entity ids and values
that are hashtables. This inner hashtable contains keys of the type of the
component and a value of a component instance. This ensures that an entity has a
unique component of a certain type.

    * ENTITY-IDS is an extendable array. The array index is an entity id and the
value is 0 or 1 indicating whether the id is active or inactive. MAKE-ENTITY
looks for an unused id and returns it, or if none are found, then it extends the
array and returns this new id.

    * SYSTEMS, is a hashtable with a key of a type of a system and a value of a
system object. The hashtable is once again used to keep track of unique types in
the world. As well as ease of getting and removing objects.
