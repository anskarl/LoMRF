# Weight Learning Examples

Below we provide some example MLNs, in order to demonstrate the LoMRF weight learning command-line tool:

## Social Network Analysis

We would like to model a simple social network of friends, smoking, and cancer. The network attempts to model friendship ties between people, their smoking habits and the causality of cancer ([see original example](http://alchemy.cs.washington.edu/tutorial/3Social_Network_Analysis.html)).

### Knowledge base (smoking.mln)

*Predicate schema:*
```lang-none
Friends(person, person)
Smokes(person)
Cancer(person)
```

*Formulas:*

```lang-none
// If you smoke then you get cancer
Smokes(x) => Cancer(x)

// People having friends who smoke, also smoke and those having friends
// who don't smoke, don't smoke.
Friends(x, y) => (Smokes(x) <=> Smokes(y))
```

Of course, this does not hold for all smokers, so in Markov logic we can just tack a weight
on to the rule, or, as we do here, learn a weight from training data.

***Training data (training.db)***

```lang-none
Friends(Anna, Bob)
Friends(Bob, Anna)
Friends(Anna, Edward)
Friends(Edward, Anna)
Friends(Anna, Frank)
Friends(Frank, Anna)
Friends(Bob, Chris)
Friends(Chris, Bob)
Friends(Chris, Daniel)
Friends(Daniel, Chris)
Friends(Edward, Frank)
Friends(Frank, Edward)
Friends(Gary, Helen)
Friends(Helen, Gary)
Friends(Gary, Anna)
Friends(Anna, Gary)

Smokes(Anna)
Smokes(Edward)
Smokes(Frank)
Smokes(Gary)

Cancer(Anna)
Cancer(Edward)
```

***Weight learning execution***

```lang-none
lomrf-wlearn -i smoking.mln -o smoking-learned.mln -t smoking-train.db -ne Smokes/1,Cancer/1
```

This produces the file smoking-learned.mln with the learned weights. Using this along with the test data, we can compute the truth value of each person smoking and getting cancer.

## Hidden Markov Models

Suppose, on a given day, we observe a car taking three actions: it is either stopped, driving, or slowing. We assume this is only dependent on the state of the stoplight in front of it: red, green or yellow. In a Markov process we need to model
`states` and `observations` at certain points in `time`. In LoMRF, we model a `state` and `observation` with a first-order predicate and `time` is a variable in each of these predicates.

### Knowledge base (traffic.mln)

*Domain and predicate schema:*

```lang-none
// Domain
state = {Stop, Drive, Slow}
obs = {Red, Green, Yellow}
time = {0,...,10}

// Predicate schema
State(state, time)
Obs(obs, time)
```

*Formulas:*

```lang-none
// In each time step, there is exactly one state and observation
State(s0, t) ^ !(s0 = s1) => !State(s1, t)
Obs(o0, t) ^ !(o0 = o1) => !Obs(o1, t)

// Probability of starting state
State(Stop, 0)
State(Drive, 0)
State(Slow, 0)

// Transition probabilities
State(Stop, t) => State(Stop, t++)
State(Stop, t) => State(Drive, t++)
State(Stop, t) => State(Slow, t++)

State(Drive, t) => State(Stop, t++)
State(Drive, t) => State(Drive, t++)
State(Drive, t) => State(Slow, t++)

State(Slow, t) => State(Stop, t++)
State(Slow, t) => State(Drive, t++)
State(Slow, t) => State(Slow, t++)

// Output probabilities
Obs(Red, t) => State(Stop, t)
Obs(Red, t) => State(Drive, t)
Obs(Red, t) => State(Slow, t)

Obs(Green, t) => State(Stop, t)
Obs(Green, t) => State(Drive, t)
Obs(Green, t) => State(Slow, t)

Obs(Yellow, t) => State(Stop, t)
Obs(Yellow, t) => State(Drive, t)
Obs(Yellow, t) => State(Slow, t)
```

*Training data (traffic-train.db):*

```lang-none
Obs(Red, 0)
State(Stop, 0)

Obs(Green, 1)
State(Drive, 1)

Obs(Yellow, 2)
State(Slow, 2)

Obs(Red, 3)
State(Stop, 3)

Obs(Green, 4)
State(Drive, 4)

Obs(Yellow, 5)
State(Slow, 5)

Obs(Red, 6)
State(Stop, 6)

Obs(Green, 7)
State(Drive, 7)

Obs(Yellow, 8)
State(Slow, 8)

Obs(Red, 9)
State(Stop, 9)

Obs(Green, 10)
State(Drive, 10)
```

***Weight learning execution***

```lang-none
lomrf-wlearn -i traffic.mln -o traffic-learned.mln -t traffic-train.db -ne State/2
```

This produces the file traffic-learned.mln with the learned weights. Using this along with the test data, we can compute the truth value of each state.
