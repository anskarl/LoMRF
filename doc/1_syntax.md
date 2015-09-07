# Syntax

LoMRF employs a logic-based language for expressing logic formulas, as well as some build-in extensions (e.g., functions and definite clauses) with additional functionality.

## Knowledge Base

The knowledge base files in LoMRF are text files having the suffix `.mln` (e.g., `file_name.mln`). The contents of a knowledge base file in LoMRF can be the following:

  1. [Optional] Domain types with their possible values.

  2. Predicate definitions, representing the structure of each predicate.

  3. [Optional] Function definitions, representing the structure of a function.

  4. [Optional] Formulas represented in [First-order logic](https://en.wikipedia.org/wiki/First-order_logic), expressing the template for producing Markov Networks.

  5. [Optional] Definite clauses (special case of formulas).


### Domain Types
Domain types with their possible values. Each domain type has some unique name and represents a particular collection of constant symbols. All constant symbols in LoMRF are finite and discrete.

Consider, for example, the domain of *time* ranging from 0 to 10. This domain is defined in LoMRF with the following statement:

```
time = {1,...,10}
```
For domains containing integer symbols, like the domain of time, we can define a sequence of numbers using the `...` symbol.  

Similarly, the domain of some *person* names is defined as:

```
person = {Achilles, Agamemnon, Menelaus, Helen, Odysseus}
```

The possible values of domain types can automatically discovered from the specified knowledge base and evidence file(s). Specifically, for each domain type, LoMRF computes the union of unique constant symbols from the knowledge base and the evidence file(s). For example, we may not explicitly specify all person names, but can be automatically discovered from the evidence files (and/or the given knowledge base).

### Predicate Definitions

Predicate definitions, express the structure of each predicate. For example, the predicate that is named with the symbol `Brothers` defines that two persons are brothers. Therefore, the definition of predicate `Brothers` has two arguments, where each one takes constants from the domain of person:

```
Brothers(person, person)
```

Similarly, the predicate `Happens` that associates the domain of *action* with *time* is defined as:

```
Happens(action, event)
```

Therefore, all predicates have some name (e.g,  `Brothers`, `Happens`, etc) and some specific number of arguments. Each argument has some domain type (e.g., `person`, `action`, `time`, etc).

### Function Definitions

Function definitions, express the structure of a function. All functions in LoMRF are finite with known domain types. Each function definition has some name, zero or more domain types as arguments and a resulting domain type.

For example, the function that is named as `monthOf` with a single argument that takes constants from the domain of time and returns constants from the domain of *month* (i.e., the month number):

 ```
 month = monthOf(time)
 ```

 Similarly a function `next` that takes a single argument from the domain of time and returns the next one is defined as follows:

 ```
 time = next(time)
 ```

### First-Order Logic Formulas

**Terms:**

Terms are intuitively represent objects and can be any of the following:
  * Constants, are starting with upper-case letter or numeric symbols, e.g., Achilles, Agamemnon, 1, 2, etc.
  * Variables, are starting with lower-case letter symbols, expressing any constant of a specific domain type, e.g., x, y, z, t, id, name, etc.
  * Functions, are starting with lower-case letter symbols, and may contain zero or more terms as arguments, e.g., motherOf(X), fatherOf(Agamemnon), etc.

**Predicates:**

Similar to any first-order logic representation, a predicate expresses a relation among its terms  that can be either True or False. In LoMRF, predicates start with an upper-case letter. For example, the statement that Agamemnon and Menelaus are brothers, is represented by the predicate `Brothers(Agamemnon, Menelaus)`. Predicates may have constants, variables and function symbols in their arguments. For example, `Brothers(Agamemnon, x)` is composed of the constant `Agamemnon` and the variable `x`.

Please note that when a predicate does not contain any variable is called *ground*. For example, the predicates `Brothers(Agamemnon, Menelaus)` and `Brothers(Atlas, fatherOf(Patroclus))` are ground, while the predicates `Brothers(x, y)`, `Brothers(Agamemnon, y)` or `Brothers(Atlas, fatherOf(y))` are not ground.

**Formulas:**

Formulas are represented in [First-order logic](https://en.wikipedia.org/wiki/First-order_logic), expressing a template for producing Markov Networks.
  * Each formula imposes a constraint over some predicates.


  * Each formula can be associated with some *weight* value, that is a positive or negative real number. The higher the value of weight, the stronger the constraint represented by the formula.
  In contrast to classical logic, all *worlds* (i.e., [Herbrand Interpretations](https://en.wikipedia.org/wiki/Herbrand_interpretation)) are possible with a certain probability. The main idea behind this is that the probability of a world increases as the number of formulas it violates decreases.



Formulas may contain one or more predicates, connected to each other with logical connectives and quantified symbols. The logical connectives and quantifiers in LoMRF are outlined in the following table:

| Symbol | Description
|:------:|:-------------------------------------------|
|  ^     | Logical conjunction (And)
|  v     | Logical disjunction (Or)
|  !     | Logical negation (Not)
|  =>    | Logical implication
| <=>    | Logical equivalence (if and only if)
| Forall | Universal quantification
| Exist  | Existential quantification

By default all variables implicitly assumed to be universally quantified unless otherwise indicated.

A knowledge base may contain both *hard and soft-constrained* formulas. Hard-constrained formulas are associated with an infinite weight value and capture the knowledge which is assumed to be certain. Therefore, an acceptable world must at least satisfy the hard constraints. Soft constraints capture imperfect knowledge in the domain, allowing for the existence of worlds in which this knowledge is violated.
  * Syntactically hard-constrained formulas, do not have weights (the weight is assumed to be infinite) and capture the knowledge which is assumed to be certain.
  * Soft-constrained formulas are always associated with weights and capture imperfect knowledge, allowing for the existence of worlds in which this knowledge is violated.


### Definite Clauses

A special case of formulas are the [definite clauses](https://en.wikipedia.org/w/index.php?title=Definite_clause), which can be used to define declarations of rules. The definite clauses, are processed by the LoMRF and automatically translated to equivalent formulas. Their syntax is simpler from the syntax of formulas and are ideal for defining domain-specific knowledge.

### Build-in functions and predicates

**Functions:**

| Function                  | Description
|:-------------------------:|:-------------------------------------------|
| x++                       | Increase the integer number *x* by one
| x--                       | Degrease the integer number *x* by one
| x + y                     | Sum *x* with *y*
| x - y                     | Subtract *y* from *x*
| x * y                     | Multiply *x* with *y*
| x / y                     | Divide *y* from *x*
| x % y                     | Modulo of *y* from *x*
| concat(x, y)              | Concatenate *x* with *y*



**Predicates:**

| Predicate                  | Description
|:--------------------------:|:-------------------------------------------|
|  a = b                     | Term *a* is equal with term *b*
|  a < b                     | Term *a* is less than term *b*
|  a <= b                    | Term *a* is less than or equal with *b*
|  a > b                     | Term *a* is greater than term *b*
|  a >= b                    | Term *a* is greater than or equal with  *b*
| substr(a, b)               | Term *a* is substring of *b*


## Evidence

Evidence files in LoMRF are text files having the suffix `.db` (e.g., `file_name.db`). The contents of an evidence file are ground predicates (facts) and optionally ground function Mappings.

### Function Mappings

A function mapping defines a possible **true** grounding of a function (see Function Definitions above). Syntactically follows the corresponding function definition in the knowledge base file, but the domain types are replaced with some of their corresponding constant symbols.  

For example, the *true* possible groundings of the function `time = next(time)` are the following:

```
2 = next(1)
3 = next(2)
4 = next(3)
5 = next(4)
6 = next(5)
7 = next(6)
8 = next(7)
9 = next(8)
10 = next(9)
```
According to the given true groundings in the example above, `next(1)` results to the constant `2`.

### Facts (ground predicates)

Ground predicates in the evidence represent known facts for the LoMRF. Each fact is expressed with predicates that contain only constants from their corresponding domains. Each fact represents a true grounding of a specific predicate, optionally facts can be negated and thus represent a false grounding of a predicate.

For example, the following ground predicates are facts that express that Agamemnon and Menelaus are brothers, but Achilles is not brother of Agamemnon.

```
Brother(Agamemnon, Menelaus)
Brother(Menelaus, Agamemnon)
!Brother(Achilles, Agamemnon)
```

For simplicity, we can also define [Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption) for specific predicates (see [Inference](2_inference.md)), and thus define only the true groundings in the evidence and assume false state for all other combinations (e.g., Brother(Achilles, Menelaus) is implicitly assumed to be false).  

## Examples

### Uniform distribution

```
flip = {1,...,20}

Heads(flip)
```

### Binomial distribution

```

```


### Multinomial distribution

### Yale Shooting Scenario
See the running example in [Quick Start](0_quick_start.md)

### Does Marcus hate Caesar?

Example in natural language:
  1. Marcus is a person.
  2. Marcus is a Pompeian.
  3. All Pompeians are Roman.
  4. Caesar is a ruler.
  5. All Romans are either loyal to Caesar or hate Caesar.
  6. Everyone is loyal to someone.
  7. People only try to assassinate rulers to whom they are not loyal.
  8. Marcus tried to assassinate Caesar.

** Knowledge base**

Predicate schema:
```lang-none
// Query predicates:
Hate(person)

// Evidence predicates:
People(person)
Ruler(person)
Pompeian(person)
Assasinate(person, person)

// Other non-evidence predicates:
Loyal(person, person)
```
Formulas:

1. All Pompeians are Roman (hard-constrained):
```lang-none
Forall x Pompeian(x) => Roman(x).
```
This formula can also written as following, since by default all variables implicitly assumed to be universally quantified unless otherwise indicated:
```lang-none
Pompeian(x) => Roman(x).
```  

2. All Romans were either loyal to Caesar or hated him or both (hard-constrained):
```lang-none
Roman(x) => Loyal(x, Caesar) v Hate(x, Caesar).
```

3. Usually, everyone is loyal to someone (soft-constrained):
```lang-none
1.10 Forall x Exists y Loyal(x,y).
```

4. People will try to assassinate rulers to whom they are not loyal (soft-constrained):
```lang-none
0.2 Forall x,y People(x) ^ Ruler(y) ^ Assasinate(x,y) => !Loyal(x, y).
```


** Evidence **

```lang-none
People(Markus)
Pompeian(Marcus)
Ruler(Caesar)
Assassinate(Marcus, Caesar)
```
