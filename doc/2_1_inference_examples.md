# Probabilistic Inference Examples

Below we provide some example MLNs, in order to demonstrate the LoMRF inference command-line tool:

## Basic distribution modeling examples

### Uniform distribution

We would like to model a simple coin flip scenario. We state that the outcome of a flip is heads with the predicate Heads(flip). Flip ranges from 1 to 20. If Heads(n) is true, then flip n was heads, otherwise it was tails ([see original example](http://alchemy.cs.washington.edu/tutorial/2_1Uniform_Distribution.html)).

Knowledge base (uniform.mln):

```lang-none
flip = {1,...,20}

Heads(flip)
```

We do not have any evidence in this example, thus we omit the `-e` parameter and we run our example with the following parameters:

```lang-none
lomrf -i uniform.mln -q Heads/1 -r uniform.result
```

The contents of the result file (uniform.result) are the following:

```lang-none
Heads(20) 0.493
Heads(19) 0.508
Heads(18) 0.515
Heads(17) 0.499
Heads(16) 0.503
Heads(15) 0.513
Heads(14) 0.496
Heads(13) 0.532
Heads(12) 0.503
Heads(11) 0.464
Heads(10) 0.494
Heads(9) 0.508
Heads(8) 0.504
Heads(7) 0.496
Heads(6) 0.527
Heads(5) 0.504
Heads(4) 0.479
Heads(3) 0.506
Heads(2) 0.473
Heads(1) 0.512
```

### Binomial distribution ###

To model a binomial distribution we simply add the weighted unit clause `1 Heads(f)` ([see original example](http://alchemy.cs.washington.edu/tutorial/2_1Uniform_Distribution.html)).

Knowledge base (binomial.mln):

```lang-none
flip = {1,...,20}

Heads(flip)

// unit clause
1 Heads(f)
```

We run our example with the following parameters:

```lang-none
lomrf -i binomial.mln -q Heads/1 -r binomial.result
```

The contents of the result file (binomial.result) are the following:

```lang-none
Heads(20) 0.728
Heads(19) 0.739
Heads(18) 0.74
Heads(17) 0.74
Heads(16) 0.718
Heads(15) 0.748
Heads(14) 0.736
Heads(13) 0.715
Heads(12) 0.76
Heads(11) 0.731
Heads(10) 0.725
Heads(9) 0.722
Heads(8) 0.749
Heads(7) 0.785
Heads(6) 0.742
Heads(5) 0.684
Heads(4) 0.742
Heads(3) 0.698
Heads(2) 0.73
Heads(1) 0.746
```

### Multinomial distribution ###

In this example we model the outcome of a six-faced die for 20 throws. The outcome of each throw with is modeled by the predicate `Outcome(throw, face)` and two formulas stating that each throw has exactly one outcome ([see original example](http://alchemy.cs.washington.edu/tutorial/2_3Multinomial_Distribution.html)).

Knowledge base (multinomial.mln):

```lang-none
throw = {1,...,20}
face = {1,...,6}

Outcome(throw, face)

// At least one outcome must occur for each throw
Exist f Outcome(t,f).

// At most one outcome must occur
Outcome(t,f0) ^ !(f0 = f1) => !Outcome(t,f1).
```

We run our example with the following parameters:
```
lomrf -i multinomial.mln -q Outcome/2 -r multinomial.result
```

The contents of the result file (multinomial.result) are the following:
```
Outcome(20,6) 0.164
Outcome(19,6) 0.151
Outcome(18,6) 0.165
Outcome(17,6) 0.158
Outcome(16,6) 0.181
Outcome(15,6) 0.148
Outcome(14,6) 0.177
Outcome(13,6) 0.158
Outcome(12,6) 0.16
Outcome(11,6) 0.177
Outcome(10,6) 0.146
Outcome(9,6) 0.175
Outcome(8,6) 0.174
Outcome(7,6) 0.153
Outcome(6,6) 0.176
Outcome(5,6) 0.169
Outcome(4,6) 0.176
Outcome(3,6) 0.164
Outcome(2,6) 0.187
Outcome(1,6) 0.152
Outcome(20,5) 0.177
Outcome(19,5) 0.18
Outcome(18,5) 0.164
Outcome(17,5) 0.173
Outcome(16,5) 0.16
Outcome(15,5) 0.18
Outcome(14,5) 0.161
Outcome(13,5) 0.146
Outcome(12,5) 0.158
Outcome(11,5) 0.161
Outcome(10,5) 0.191
Outcome(9,5) 0.155
Outcome(8,5) 0.165
Outcome(7,5) 0.162
Outcome(6,5) 0.172
Outcome(5,5) 0.166
Outcome(4,5) 0.165
Outcome(3,5) 0.169
Outcome(2,5) 0.166
Outcome(1,5) 0.189
Outcome(20,4) 0.184
Outcome(19,4) 0.2
Outcome(18,4) 0.179
Outcome(17,4) 0.164
Outcome(16,4) 0.17
Outcome(15,4) 0.181
Outcome(14,4) 0.172
Outcome(13,4) 0.168
Outcome(12,4) 0.159
Outcome(11,4) 0.142
Outcome(10,4) 0.156
Outcome(9,4) 0.179
Outcome(8,4) 0.163
Outcome(7,4) 0.164
Outcome(6,4) 0.163
Outcome(5,4) 0.155
Outcome(4,4) 0.164
Outcome(3,4) 0.169
Outcome(2,4) 0.16
Outcome(1,4) 0.161
Outcome(20,3) 0.157
Outcome(19,3) 0.168
Outcome(18,3) 0.175
Outcome(17,3) 0.165
Outcome(16,3) 0.16
Outcome(15,3) 0.174
Outcome(14,3) 0.158
Outcome(13,3) 0.172
Outcome(12,3) 0.18
Outcome(11,3) 0.166
Outcome(10,3) 0.163
Outcome(9,3) 0.167
Outcome(8,3) 0.176
Outcome(7,3) 0.165
Outcome(6,3) 0.156
Outcome(5,3) 0.181
Outcome(4,3) 0.158
Outcome(3,3) 0.178
Outcome(2,3) 0.176
Outcome(1,3) 0.18
Outcome(20,2) 0.166
Outcome(19,2) 0.141
Outcome(18,2) 0.148
Outcome(17,2) 0.166
Outcome(16,2) 0.171
Outcome(15,2) 0.135
Outcome(14,2) 0.174
Outcome(13,2) 0.176
Outcome(12,2) 0.171
Outcome(11,2) 0.176
Outcome(10,2) 0.175
Outcome(9,2) 0.158
Outcome(8,2) 0.164
Outcome(7,2) 0.169
Outcome(6,2) 0.163
Outcome(5,2) 0.161
Outcome(4,2) 0.176
Outcome(3,2) 0.159
Outcome(2,2) 0.16
Outcome(1,2) 0.171
Outcome(20,1) 0.152
Outcome(19,1) 0.16
Outcome(18,1) 0.169
Outcome(17,1) 0.174
Outcome(16,1) 0.158
Outcome(15,1) 0.182
Outcome(14,1) 0.158
Outcome(13,1) 0.18
Outcome(12,1) 0.172
Outcome(11,1) 0.178
Outcome(10,1) 0.169
Outcome(9,1) 0.166
Outcome(8,1) 0.158
Outcome(7,1) 0.187
Outcome(6,1) 0.17
Outcome(5,1) 0.168
Outcome(4,1) 0.161
Outcome(3,1) 0.161
Outcome(2,1) 0.151
Outcome(1,1) 0.147
```

## Does Marcus hate Caesar?

Description in natural language:
  1. Marcus is a person.
  2. Marcus is a Pompeian.
  3. All Pompeians are Roman.
  4. Caesar is a ruler.
  5. All Romans are either loyal to Caesar or hate Caesar.
  6. Everyone is loyal to someone.
  7. People only try to assassinate rulers to whom they are not loyal.
  8. Marcus tried to assassinate Caesar.

### Knowledge base (theory.mln)

*Predicate schema:*
```lang-none
// Query predicates:
Hate(person,person)

// Evidence predicates:
People(person)
Ruler(person)
Pompeian(person)
Assassinate(person, person)

// Other non-evidence predicates:
Loyal(person, person)
Roman(person)
```

*Formulas:*


* All Pompeians are Roman (hard-constrained):

```lang-none
Forall x Pompeian(x) => Roman(x).
```
This formula can also written as following, since by default all variables implicitly assumed to be universally quantified unless otherwise indicated:

```lang-none
Pompeian(x) => Roman(x).
```

* All Romans were either loyal to Caesar or hated him or both (hard-constrained):

```lang-none
Roman(x) => Loyal(x, Caesar) v Hate(x, Caesar).
```

* Usually, everyone is loyal to someone (soft-constrained):

```lang-none
1 Forall x Exist y Loyal(x, y)
```

* People may try to assassinate rulers to whom they are not loyal (soft-constrained):

```lang-none
2 Forall x,y People(x) ^ Ruler(y) ^ Assassinate(x,y) => !Loyal(x, y)
```
* Usually nobody hates himself (soft-constrained):

```lang-none
1 !Hate(x, x)
```

*Final form of the knowledge base file (theory.mln):*

```lang-none
// Query predicates:
Hate(person,person)

// Evidence predicates:
People(person)
Ruler(person)
Pompeian(person)
Assassinate(person, person)

// Other non-evidence predicates:
Loyal(person, person)
Roman(person)

// All Pompeians are Roman (hard-constrained)
Pompeian(x) => Roman(x).

// All Romans were either loyal to Caesar or hated him or both (hard-constrained)
Roman(x) => Loyal(x, Caesar) v Hate(x, Caesar).

// Usually, everyone is loyal to someone (soft-constrained)
1 Exist y Loyal(x,y)

// People may try to assassinate rulers to whom they are not loyal (soft-constrained)
2 People(x) ^ Ruler(y) ^ Assassinate(x,y) => !Loyal(x, y)

// Usually, nobody hates himself (soft-constrained)
1 !Hate(x, x)
```

***Evidence (evidence.db)***

```lang-none
People(Markus)
Pompeian(Marcus)
Ruler(Caesar)
Assassinate(Marcus, Caesar)
```

### Inference

***Inference execution***

```lang-none
lomrf -infer marginal -i theory.mln -e evidence.db -r output.result -q Hate/2 -owa Loyal/2,Roman/1 -cwa People/1,Ruler/1,Pompeian/1,Assassinate/2
```

***Inference result (output.result)***

```lang-none
Hate(Marcus,Marcus) 0.251
Hate(Caesar,Marcus) 0.532
Hate(Marcus,Caesar) 0.855
Hate(Caesar,Caesar) 0.315
```
