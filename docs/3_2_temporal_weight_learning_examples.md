# Temporal Weight Learning Examples

Below we provide examples that demonstrate LoMRF weight learning capabilities in the domain of temporal reasoning. Recall that sources from the following examples are located in the [LoMRF-data](https://github.com/anskarl/LoMRF-data) project (follow the instructions in [Download Example Data](7_2_download_example_data.md)).

## Activity Recognition

In this example we demonstrate how to perform weight learning for activity recognition, using a small fragment of the first set of the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/). We use the approach that is proposed by [Skarlatidis et. al. (2014, 2015)](9_references.md), that employs the Probabilistic Event Calculus formalism as presented in the Section [Quick Start](0_quick_start.md) and the knowledge base that is defined in the [Temporal Inference Examples](2_2_temporal_inference_examples.md).

Recall that in Section [Knowledge Base](1_1_knowledge_base.md) we presented a special type of formulas, named [Definite Clauses](1_1_knowledge_base.md#definite-clauses), as well as their properties and their limitations regarding predicate completion (for details see the works of [McCarthy, 1980](9_references.md) and [Lifschitz, 1994](9_references.md)).

### Training data

The training data is composed of ground facts of the input event predicates `StartTime/1`, `Happens/2`, `Close/4`, `OrientationMove/3`, ground function mappings (e.g., for the functions `enter/1`, `walking/1`, `running/1`, etc.), as well as ground facts of the annotation predicates `HoldsAt/2`.

For example, consider the following fragment:

```lang-none
// Input events:
Enter_ID0 = enter(ID0)
Enter_ID1 = enter(ID1)
Exit_ID0 = exit(ID0)
Exit_ID1 = exit(ID1)
Walking_ID0 = walking(ID0)
Walking_ID1 = walking(ID1)
Running_ID0 = running(ID0)
Running_ID1 = running(ID1)
Active_ID0 = active(ID0)
Active_ID1 = active(ID1)
Inactive_ID0 = inactive(ID0)
Inactive_ID1 = inactive(ID1)

// Output composite events (fluents):
Move_ID0_ID0 = move(ID0, ID0)
Move_ID0_ID1 = move(ID0, ID1)
Move_ID1_ID0 = move(ID1, ID0)
Move_ID1_ID1 = move(ID1, ID1)

Meet_ID0_ID0 = meet(ID0, ID0)
Meet_ID0_ID1 = meet(ID0, ID1)
Meet_ID1_ID0 = meet(ID1, ID0)
Meet_ID1_ID1 = meet(ID1, ID1)

// Facts
StartTime(0)

HoldsAt(Meet_ID0_ID1, 174)
HoldsAt(Meet_ID0_ID1, 175)
HoldsAt(Meet_ID0_ID1, 176)
//
// ... sequence of facts ...
//
Happens(Walking_ID0, 100)
Happens(Walking_ID1, 100)
OrientationMove(ID0, ID1, 100)
Close(ID0, ID1, 34, 100)
//
// ... sequence of facts ...
//
Happens(Active_ID0, 170)
Happens(Active_ID1, 170)
//
// ... sequence of facts ...
```


### Knowledge Base Pre-processing Step

As we have seen in [Temporal Inference Examples](2_2_temporal_inference_examples.md), the knowledge base contains, among others, the predicates with signatures `InitiatedAt/2` and `TerminatedAt/2`. These predicates form the head of the definite clauses that represent the domain-dependent axioms in our Activity Recognition example. However the supervision of `InitiatedAt/2` and `TerminatedAt/2` is missing from the training data. Therefore, `InitiatedAt/2` and `TerminatedAt/2` from hidden variables in the resulting Markov Network and supervised learning cannot be performed.

To overcome this problem, we are apply a Knowledge Base transformation step (for details see [Skarlatidis et. al (2014, 2015)](9_references.md)). In brief, all logical definitions of `InitiatedAt/2` and `TerminatedAt/2` can be represented as equivalences for each composite event separately. Consider, for example, the following fragment of definitions for the composite event *meeting*, without any weight value:

```lang-none
InitiatedAt(meet(p1,p2), t) :- Happens(active(p1), t) ^ !Happens(running(p2), t) ^  Close(p1,p2,25,t)

InitiatedAt(meet(p1,p2),t) :- Happens(inactive(p1),t) ^ !Happens(running(p2),t)  ^ !Happens(active(p2),t) ^ Close(p1,p2,25,t)
```

This can be written as:
```lang-none
InitiatedAt(meet(p1,p2), t) <=> (Happens(active(p1), t) ^ !Happens(running(p2), t) ^  Close(p1,p2,25,t)) v (Happens(inactive(p1),t) ^ !Happens(running(p2),t)  ^ !Happens(active(p2),t) ^ Close(p1,p2,25,t))
```

Furthermore, each Event Calculus domain-independent axiom can be specialised according to the constants and function symbols that appear in the head predicate of each domain-dependent definition. In our example, for instance, the head predicate `InitiatedAt(meet(p1,p2), t)` contains the function symbol `meet/2`. Therefore, we can specialise the Event Calculus domain-independent axiom by substituting the `fluent` variable with the function symbol `meet/2`:

```lang-none
// the original inertia axiom
!HoldsAt(f, t) ^ !InitiatedAt(f, t) => !HoldsAt(f, t++).
// is specialised as:
!HoldsAt(meet(p1,p2), t) ^ !InitiatedAt(meet(p1,p2), t) => !HoldsAt(meet(p1,p2), t++).
```
This process is being applied for all head predicates of all definite clauses.

Finally, we can replace `InitiatedAt(meet(p1,p2), t)` with the corresponding equivalence. With that process, the knowledge base is translated into an equivalent one and the `InitiatedAt/2` and `TerminatedAt/2` predicates are completely eliminated. As a result, we do not need the supervision for `InitiatedAt/2` and `TerminatedAt/2` and the training problem is now fully supervised.

In brief, LoMRF can automatically rewrite definite clause definitions, in order to create such equivalences and simplify the knowledge base by applying the following transformation:
1. Rewrite all definite clauses as equivalencies.
2. All axioms that contain the target head predicates are specialised, with respect to the constants and function that appear in the head predicates.
3. Replace all predicates with their equivalencies.
4. (optional step) Compute the [Conjunctive Normal Form (CNF)](https://en.wikipedia.org/wiki/Conjunctive_normal_form) of the theory.

All these steps can be applied by the `lomrf compile` command-line tool of LoMRF:
```lang-none
lomrf compile -i theory.mln -o theory_cnf.mln -cnf -w remove_all -pcm simplification
```
Where '-i theory.mln' is the original input theory MLN file, '-o theory_cnf.mln' is the resulting output file, '-cnf' specifies that all clauses in the resulting theory are in CNF form, '-w remove_all' specifies that all weights from formulas will be removed and '-pcm simplification' applied the whole transformation step (i.e., all first three steps).


### Weight Learning

Once we have applied the preprocessing step, we can proceed to the weight learning step. Recall that sources from the examples are located in the [LoMRF-data](https://github.com/anskarl/LoMRF-data) project (follow the instructions in [Download Example Data](7_2_download_example_data.md)). The files of this example are the following:
  * Knowledge base files:
    * Main MLN file in CNF: `Data/Examples/Weight_Learning/Activity_Recognition/theory_cnf.mln`
    * Definitions of moving activity: `Data/Examples/Weight_Learning/Activity_Recognition/definitions/moving.mln`
    * Definitions of meeting activity: `Data/Examples/Weight_Learning/Activity_Recognition/definitions/meeting.mln`
  * Training file for batch learning: `Data/Examples/Weight_Learning/Activity_Recognition/training/batch/training.db`
  * Training files for online learning: `Data/Examples/Weight_Learning/Activity_Recognition/training/online/`


**Parameters**:
 * Non-evidence predicates: `-ne HoldsAt/2`
 * Input MLN theory: `-i theory_cnf.mln`
 * Input training data: `-t training.db`
 * Resulting output MLN theory: `-o learned.mln`
 * Enable loss augmented inference (also known as separation oracle) using the Hamming loss function by adding to the objective function during inference additional loss terms: `-lossAugmented`
 * Specify the learning algorithm, i.e., Max-Margin (default), Adagrad or CDA: `-alg`

***Max-Margin Learning***

```lang-none
lomrf wlearn -alg MAX_MARGIN -i theory_cnf.mln -t training.db -o learned.mln -ne HoldsAt/2 -lossAugmented
```
***Online Learning using AdaGrad or CDA***

```lang-none
lomrf wlearn -alg ADAGRAD -i theory_cnf.mln -t ./training/online -o learned.mln -ne HoldsAt/2

lomrf wlearn -alg CDA -i theory_cnf.mln -t ./training/online -o learned.mln -ne HoldsAt/2 -lossAugmented
```