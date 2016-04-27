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

### Weight Learning

***Weight learning execution***

```lang-none
lomrf-wlearn -i smoking.mln -o smoking-out.mln -t smoking-train.db -ne Smokes/1,Cancer/1
```

This produces the file smoking-out.mln with the learned weights. Using this along with the test data, we can compute the marginal probabilities of each person smoking and getting cancer
