# Weight Learning Examples

Below we provide some example MLNs, in order to demonstrate the LoMRF weight learning command-line tool:

## Social Network Analysis

We would like to model a simple social network of friends, smoking, and cancer. The network attempts to model friendship ties between people, their smoking habits and the causality of cancer ([see original example](http://alchemy.cs.washington.edu/tutorial/3Social_Network_Analysis.html).

Knowledge base (smoking.mln):

```lang-none
Friends(person, person)
Smokes(person)
Cancer(person)

// If you smoke then you get cancer
Smokes(x) => Cancer(x)

// People having friends who smoke, also smoke and those having friends
// who don't smoke, don't smoke
Friends(x, y) => (Smokes(x) <=> Smokes(y))
```

***Training data (training.db)***

```lang-none
Friends(Anna, Bob)
Friends(Bob, Anna)
Friends(Anna, Alex)
Friends(Alex, Anna)
Friends(Anna, Frank)
Friends(Frank, Anna)
Friends(Bob, Tony)
Friends(Tony, Bob)
Friends(Tony, Bernard)
Friends(Bernard, Tony)
Friends(Alex, Frank)
Friends(Frank, Alex)
Friends(Gary, Helen)
Friends(Helen, Gary)
Friends(Gary, Anna)
Friends(Anna, Gary)

Smokes(Anna)
Smokes(Tony)
Smokes(Frank)
Smokes(Gary)

Cancer(Anna)
Cancer(Tony)
```
