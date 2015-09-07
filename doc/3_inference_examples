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
