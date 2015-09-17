# Temporal Probabilistic Inference Examples

Below we provide example MLNs that demonstrate some LoMRF advanced inference capabilities in the domain of temporal reasoning.

## Yale Shooting Scenario ##

For a quick introduction to temporal representation (using the Probabilistic Event Calculus formalism, see [Skarlatidis et. al. (2014, 2015)](#references)) and reasoning,
see the temporal reasoning example in [Quick Start](0_quick_start.md) section.

## Activity Recognition ##

In this example we demonstrate how to perform probabilistic activity recognition, using a small fragment of
the first set of the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/). We use the same
Probabilistic Event Calculus formalism as presented in the [Quick Start](0_quick_start.md) section.

The aim in activity recognition is to recognise complex activities that take place between multiple persons,
by exploiting information about simple observed individual activities. The first set [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/)
comprises 28 surveillance videos, where each frame is annotated by human experts from the CAVIAR team on two levels.
The first level contains simple, input events that concern activities of individual persons or the state of objects.
The second level contains composite event annotations, describing the activities between multiple persons and/or
objects - i.e., people *meeting* and *moving* together, *leaving an object* and *fighting*.

Below we briefly describe the input of the activity recognition example:

 1. The input is composed of a collection of input events, representing people *walking*, *running*, *staying active*,
 or *inactive*. The first and the last time that a person or an object is tracked are represented by the
 input events *enter* and *exit*. Additionally, the coordinates of tracked persons or objects are also taken into
 consideration, in order to express qualitative spatial relations, *e.g., two persons being relatively close to
 each other*.

 2. In the following sub-section we give the composite event definitions of *meeting*, *moving*,
 *leaving an object* and *fighting*. These definitions take the form of common sense rules and describe the
 conditions under which a CE starts or ends. For example, when two persons are walking together with the
 same orientation, then moving starts being recognised. Similarly, when the same persons walk away from
 each other, then moving stops being recognised.

Based on a collection of input events and composite event definitions, we would like to perform probabilistic
inference and recognise instances of the composite events of interest.

### Domain types

In this application we have the following domain types:

| Domain type   | Description                                                                                                                                  |
|:--------------|:---------------------------------------------------------------------------------------------------------------------------------------------|
| id            | domain of constants that uniquely identify persons, and possibly objects, that appear in the scene, e.g., constants *ID1*, *ID2*, etc.       |
| dist          | domain of numeric thresholds, that indicate relative distances between persons and/or objects in the scene.                                  |
| time          | domain of time, representing the video frame number.                                                                                         |
| events        | domain of constants that uniquely identify the occurrence of **input events** of persons and objects in the scene.                           |
| fluents       | domain of constants that uniquely identify the recognition of **output composite events** between pairs of persons and objects in the scene. |

### Function schemas

Input events occur over single person/objects that appear in the scene. We represent them in LoMRF as
term functions (see Function Definitions in [Syntax](1_syntax.md) section).

| Input Events  | Description                |
|:--------------|:---------------------------|
| walking(x)    | person *x* is walking |             
| running(x)    | person *x* is running |
| active(x)     | person *x* is active, e.g., moving her arms, while staying at the same position |
| inactive(x)   | person *x* is inactive, i.e., standing still |
| enter(x)      | person *x* enters the scene |
| exit(x)       | person *x* exits from the scene|

In particular, the schema of term functions that represent the input events is given below:

```lang-none
event walking(id)
event running(id)
event active(id)
event inactive(id)
event enter(id)
event exit(id)
```

Output composite events are defined over pairs of person/objects that appear in the scene. We represent them in LoMRF as
term functions (see Function Definitions in [Syntax](1_syntax.md) section).

| Output Composite Events  | Description                              |
|:-------------------------|:-----------------------------------------|
| move(x,y)                | persons *x* and *y* are moving together  |
| meet(x,y)                | persons *x* and *y* are meeting          |
| fight(x,y)               | persons *x* and *y* are fighting         |
| leaving_object(x,y)      | person *x* is leaving a object *y*       |

The schema of term functions that represent the output composite events is given below:

```lang-none
fluent move(id,id)
fluent meet(id,id)
fluent fight(id,id)
fluent leaving_object(id,id)
```

### Predicate schemas

The predicates that we use follow the definition of the Probabilistic Event Calculus ([Skarlatidis et. al. 2014, 2015](#references)) formalism. We additionally
use some utility predicates in order to represent some spacial constraints.

| Predicate                  | Meaning                                                                                                   |
|:---------------------------|:----------------------------------------------------------------------------------------------------------|
| Happens(e, t)              | An input event *e* occurs at some point at time *t*                                                       |
| HoldsAt(f, t)              | An output composite event *f* holds at some point at time *t*                                             |
| InitiatedAt(f, t)          | An output composite event *f* is initiated at some point at time *t*, i.e., begins to hold                |
| TerminatedAt(f, t)         | An output composite event *f* is terminated at some point at time *t*, i.e., stops to hold                |
| Close(x,y,d,t)             | The relative distance between the persons/objects *x* and *y* is below the threshold *d* at time *t*      |
| OrientationMove(x,y,t)     | Persons *x* and *y* have similar orientation at time *t*                                                  |
| StartTime(t)               | Utility predicate, stating the beginning of time. We use it to represent the initial state of the example |

The schema of the predicates that we use in this example it defined in LoMRF as below:
```lang-none
Happens(event, time)
HoldsAt(fluent, time)
InitiatedAt(fluent, time)
TerminatedAt(fluent, time)
Close(id, id, dist, time)
OrientationMove(id, id, time)
StartTime(time)
```

### Domain-independent definitions of the Probabilistic Event Calculus formalism

So far we have defined the domain types, as well as the predicate and term function schemas. Below we define the
domain-independent axioms of the probabilistic Event Calculus formalism.

```lang-none
InitiatedAt(f, t) => HoldsAt(f, t++).

TerminatedAt(f, t) => !HoldsAt(f, t++).

HoldsAt(f, t) ^ !TerminatedAt(f, t) => HoldsAt(f, t++).

!HoldsAt(f, t) ^ !InitiatedAt(f, t) => !HoldsAt(f, t++).
```
For details see the Domain-independent axioms sub-section in [Quick Start](0_quick_start.md).

We additionally add the following formula, in order to state that at the beginning of time nothing holds:

```lang-none
// initially nothing holds:
StartTime(t) => !HoldsAt(f, t).
```

### Domain-dependent definitions

Below we give the definitions of composite event definitions that express long-term behaviour patterns of interest.
We assume that we know the weight values (e.g., expert knowledge or we have performed weight learning). For simplicity
and compactness, we only present the definitions of *moving* and *meeting*.

**People are moving together**

According to the definitions below, moving composite event is initiated when two persons *p1* and *p2*
are walking close to each other (their distance is at most 34 pixels) with almost the same orientation.
The composite event is terminated under several cases: (a) When people walk away from each other, i.e.,
they have a distance larger than 34 pixels, (b) when they stop moving, i.e., either both are active,
or one is active while the other is inactive. (c) When one of them is running or exiting the scene.

```lang-none
// When people begin to move together

1.386 InitiatedAt(move(p1,p2), t) :- Happens(walking(p1), t) ^ Happens(walking(p2), t) ^ OrientationMove(p1,p2,t) ^ Close(p1,p2,34,t)

// When people stop moving together

// --- walk away
2 TerminatedAt(move(p1,p2), t) :- Happens(walking(p1), t) ^ !Close(p1,p2,34,t)
2 TerminatedAt(move(p1,p2), t) :- Happens(walking(p2), t) ^ !Close(p2,p1,34,t)

// --- both are active
2 TerminatedAt(move(p1,p2), t) :- Happens(active(p1), t) ^ Happens(active(p2),t)

// --- one is active and the other is inactive
2 TerminatedAt(move(p1,p2), t) :- Happens(active(p1), t)  ^ Happens(inactive(p2),t)
2 TerminatedAt(move(p1,p2), t) :- Happens(active(p2), t)  ^ Happens(inactive(p1),t)

// --- start running
2 TerminatedAt(move(p1,p2), t) :- Happens(running(p1), t)
2 TerminatedAt(move(p1,p2), t) :- Happens(running(p2), t)

// --- exit (hard-constrained)
TerminatedAt(move(p1,p2), t) :- Happens(exit(p1),t).
TerminatedAt(move(p1,p2), t) :- Happens(exit(p2),t).
```

**People are meeting**

According to the definitions below, meeting is initiated when the people involved interact with each other,
i.e., at least one of them is active or inactive, the other is not running, and the measured distance between
them is at most 25 pixels. The meeting composite event is terminated when people walk away from each other,
or when one of them is running, or has exited the scene.

```lang-none
// When people begin to meet

1.386 InitiatedAt(meet(p1,p2), t) :- Happens(active(p1), t) ^ !Happens(running(p2), t) ^  Close(p1,p2,25,t)

-3.178 InitiatedAt(meet(p1,p2),t) :- Happens(inactive(p1),t) ^ !Happens(running(p2),t)  ^ !Happens(active(p2),t) ^ Close(p1,p2,25,t)

// When people stop meeting

// --- walking
2 TerminatedAt(meet(p1,p2),t) :- Happens(walking(p1),t)  ^ !Close(p1,p2,34,t)
2 TerminatedAt(meet(p1,p2),t) :- Happens(walking(p2),t)  ^ !Close(p2,p1,34,t)

// --- start running
2 TerminatedAt(meet(p1,p2),t)  :- Happens(running(p1),t)
2 TerminatedAt(meet(p1,p2),t) :- Happens(running(p2),t)

// --- exit (hard-constrained)
TerminatedAt(meet(p1,p2),t) :- Happens(exit(p1), t).
TerminatedAt(meet(p1,p2),t) :- Happens(exit(p2), t).
```

### Evidence

The evidence is composed of ground facts of the input predicates `StartTime/1`, `Happens/2`, `Close/4`, `OrientationMove/3`,
as well as ground function mappings. For example, consider the following fragment:

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

### Inference

The files of this examples are the following:
  * Knowledge base files:
    * Main MLN file: [theory.mln](/Examples/data/Activity_Recognition/theory.mln)
    * Definitions of moving activity: [definitions/moving.mln](/Examples/data/Activity_Recognition/definitions/moving.mln)
    * Definitions of meeting activity: [definitions/meeting.mln](/Examples/data/Activity_Recognition/definitions/meeting.mln)
  * Evidence file: [narrative.db](/Examples/data/Activity_Recognition/narrative.db)

Parameters:
 * Query predicates: `HoldsAt/2`
 * Evidence predicates (Closed-world assumtion): `StartTime/1`, `Happens/2`, `Close/4` and `OrientationMove/3`.

***Marginal inference***

```lang-none
lomrf -infer marginal -i theory.mln -e narrative.db -r marginal-out.result -q HoldsAt/2 -cwa StartTime/1,Happens/2,Close/4,OrientationMove/3
```
***MAP inference***

```lang-none
lomrf -infer map -i theory.mln -e narrative.db -r map-out.result -q HoldsAt/2 -cwa StartTime/1,Happens/2,Close/4,OrientationMove/3
```

## References

Skarlatidis A. Event Recognition Under Uncertainty and Incomplete Data. (2014). PhD Thesis. Department of Digital Systems, University of Piraeus. ([link](http://hdl.handle.net/10442/hedi/35692))

Skarlatidis A., Paliouras G., Artikis A. and Vouros G. (2015). Probabilistic Event Calculus for Event Recognition. ACM
Transactions on Computational Logic, 16, 2, Article 11, pp. 11:1-11:37. ([link](http://dx.doi.org/10.1145/2699916))
