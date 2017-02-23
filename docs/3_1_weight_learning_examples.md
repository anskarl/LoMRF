# Weight Learning Examples

Below we provide simple example models, in order to demonstrate weight learning in LoMRF. Recall that sources from the following examples are located in the [LoMRF-data](https://github.com/anskarl/LoMRF-data) project (follow the instructions in [Download Example Data](7_2_download_example_data.md)).

## Social Network Analysis

We would like to model a simple social network of friends, smoking, and cancer. The network attempts to model friendship ties between people, their smoking habits and the causality of cancer (see [original example](http://alchemy.cs.washington.edu/tutorial/3Social_Network_Analysis.html)).

The sources of this example can be found in `Data/Examples/Weight_Learning/Friends_Smokers` in the sub-module **Data** (see the instructions in Section [Download Example Data](7_2_download_example_data.md)).


### Knowledge Base

In the first part of the knowledge base we should define the *schema* of each predicate that we are using in the example (for details see the instructions in Section [Knowledge Base](1_1_knowledge_base.md)).

*Predicate schema:*
```lang-none
// Input predicates:
Friends(person, person)

// Query/non-evidence predicates:
Smokes(person)
Cancer(person)
```
In the second part we give the *formulas*, using first-order logic syntax, in order to define the relations and the constraints of our example model.

*Formulas:*

```lang-none
// If you smoke then you get cancer
Smokes(x) => Cancer(x)

// People having friends who smoke, also smoke and those having friends
// who don't smoke, they don't smoke.
Friends(x, y) => (Smokes(x) <=> Smokes(y))
```

Of course, this cannot not hold for all smokers and their friends with absolute certainty. In Markov Logic we can associate a weight value to each logical formula, or use weight learning in order to estimate the weights from training data.

Please note, that both example formulas are not hard-constrained (i.e., ending with a full-stop character). On the other hand, although they are both soft-constrained, they are not (yet) associated with a weight value. *The lack of weight value, indicates that the weight needs to be estimated by the weight learning algorithm*.

### Training data (smoking-train.db)

In the following training data we give example relations between friends, e.g., the fact that the persons `Anna` and `Bob` are friends (using the true ground fact `Friends(Anna, Bob)`). Furthermore, we define who is a smoker, e.g., `Anna` is a smoker, therefore we are givind the true ground fact `Smokes(Anna)`. Similarly, we define which persons are diagnosed with cancer, e.g., `Canser(Anna)`. Please note, that due to [Closed-world](https://en.wikipedia.org/wiki/Closed-world_assumption) assumption in evidence we do not necessary need to give which possible is false, e.g., the fact that `Bob` is not a smoker (i.e., `!Smokes(Bob)`).

Below we are giving the full example of our training data:

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

In order to perform weight learning for this example we are giving the following:

```lang-none
lomrf wlearn -i smoking.mln -o smoking-learned.mln -t smoking-train.db -ne Smokes/1,Cancer/1
```
Where the parameter '-i smoking.mln' is the input MLN theory, '-o smoking-learned.mln' is the resulting output theory with the estimated weights, '-t smoking-train.db' is the training data and the parameer '-ne Smokes/1,Cancer/1' specifies which predicates are the non-evidence predicates. After the execution of this example, the resulting file `smoking-learned.mln` is an MLN knowledge base with the learned weights. Using this file along with the test data, we can compute the truth value of each person smoking and getting cancer.

## Car Traffic

In the following example we demonstrate weight learning using a *simple* implementation of [Hidden Markov Model](https://en.wikipedia.org/wiki/Hidden_Markov_model) for modelling car traffic (see [original example](http://alchemy.cs.washington.edu/tutorial/7Hidden_Markov_Models.html)).
We assume that each day a car may take one of the following actions (1) *stopped*, (2) *driving*, (3) or *slowing down*. Furthermore, we assume that these actions are dependent by the state of the stoplight in front of it, which can be either *red*, *green* or *yellow*.

In a Markov process we need to model `states` and `observations` at certain points in `time`. Using first-order logic representation we can model a `state` and `observation` using predicates. On the other hand, *time*, *car states* and *traffic light observations* are represented as variables in each one of these predicates.

The sources of this example can be found in `Data/Examples/Weight_Learning/Car_Traffic` in the sub-module **Data** (see the instructions in Section [Download Example Data](7_2_download_example_data.md)).

### Knowledge base.

Please find below the example knowledge base (file `traffic.mln`):

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

### Training data

Please find below the example training data (file `traffic-train.db`):

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
lomrf wlearn -i traffic.mln -o traffic-learned.mln -t traffic-train.db -ne State/2
```

This produces the file `traffic-learned.mln` with the learned weights. Using the resulting trained MLN model along with the test data (file `traffic-test.db`), we can compute the truth value of each state.
