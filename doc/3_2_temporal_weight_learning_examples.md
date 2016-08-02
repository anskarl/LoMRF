# Temporal Weight Learning Examples

Below we provide examples that demonstrate LoMRF weight learning capabilities in the domain of temporal reasoning.

## Activity Recognition ##

In this example we demonstrate how to perform weight learning for activity recognition, using a small fragment of the first
set of the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/). We use the same Probabilistic Event Calculus
formalism as presented in the [Quick Start](0_quick_start.md) section and the same knowledge base as the one defined in
the [Temporal Inference Examples](2_2_temporal_inference_examples.md).

###Training data

The training data is composed of ground facts of the input predicates `StartTime/1`, `Happens/2`, `Close/4`, `OrientationMove/3`,
ground facts of the annotation predicates `HoldsAt/2`, as well as ground function mappings. For example, consider the following fragment:

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