## Online Structure Learning Examples

Below we provide online structure learning examples for OSL and OSLa, using the LoMRF structure learning command-line tool:

## OSL in Natural Language Processing

We would like to perform an information extraction task, called field segmentation, a real-world problem where the data
contain many, relatively small, structured examples in the form of short documents. A document is represented as sequence
of tokens, and the goal is to segment the text into fields, i.e. label each token in the document with a particular field.
We use the CiteSeer dataset that contains 1563 bibliographic citations. The dataset has four disjoint subsets consisting
of citations in four different research areas. The task is to segment each citation into three fields: Author, Title and Venue.

### Knowledge base (empty.mln)

We begin from an empty knowledge base that specify only the predicate schema.

*Predicate schema:*
```lang-none
// Predicate definitions
Token(token,position,bib)
FollowBy(bib,position,token)
Next(position,position)
LessThan(position,position)
IsAlphaChar(token)
IsDate(token)
IsDigit(token)
FirstNonAuthorTitleTkn(bib,position)
FirstIn(bib,position)
LastInitial(bib,position)
Center(bib,position)
HasPunc(bib,position)
HasComma(bib,position)

// Non-Evidence
InField(bib,field,position)
```

### Mode Declarations (citeceer.modes)

We define a set of mode declarations, a form of language bias, to constrain the
structure of clauses learned. Each mode definitions begins either with the symbol `modeP`
or `modeF` to specify whether it refers to a predicate or function. In this example we
only have predicates and therefore function mode declarations are not required. Each mode
declaration has a recall number (1st argument), that restricts the number of times the
specific predicate can appear in a clause, and a predicate symbol having as arguments
placemarkers. Placemarkers can be input `+`, output `-`, or ignore `.`.Finally, symbol
`#` specifies that this argument will remain constant after varabilization.

*Mode declarations:*
```lang-none
modeP(2, Token(#-,+,.))
modeP(2, FollowBy(.,+,#.))
modeP(1, Next(-,-))
modeP(0, LessThan(-,-))
modeP(2, IsAlphaChar(#+))
modeP(2, IsDate(#+))
modeP(2, IsDigit(#+))
modeP(2, FirstNonAuthorTitleTkn(.,+))
modeP(2, FirstIn(.,+))
modeP(2, LastInitial(.,+))
modeP(2, Center(.,+))
modeP(2, HasPunc(.,+))
modeP(2, HasComma(.,+))
modeP(2, InField(.,#.,+))
```

### Training micro-batches

The training data is composed of ground facts of the input predicates `Token/3`, `IsDigit/1`, `IsAlphaChar/1`, `IsDate/1`, `FirstNonAuthorTitleTkn/2`, `FollowBy/3`
`FirstIn/2`, `LastInitial/2`, `Center/2`, `HasPunc/2`, `HasComma/2`, `Next/2` and ground facts of the annotation predicates `InField/3`.

```lang-none
IsDigit(T0044)
IsDigit(T01)
IsAlphaChar(Tg)
IsAlphaChar(Th)
IsDate(Tjanuary)
IsDate(Tjuly)
//
// ... sequence of facts ...
//
Next(P01,P00)
Next(P02,P01)
//
// ... sequence of facts ...
//
Token(Tj,P00,B0013)
FollowBy(B0013,P00,TPERIOD)
InField(B0013,Fauthor,P00)
Token(Tjaffar,P01,B0013)
FollowBy(B0013,P01,TCOMMA)
HasPunc(B0013,P01)
HasComma(B0013,P01)
//
// ... sequence of facts ...
//
LastInitial(B0013,P00)
FirstIn(B0013,P08)
FirstNonAuthorTitleTkn(B0013,P19)
Center(B0013,P04)
```

### Structure Learning

The files of this example are the following:
  * Initially empty MLN file: [empty.mln](/Examples/Structure_Learning/OSL_NLP/empty.mln)
  * Mode declaration file: [citeceer.mln](/Examples/Structure_Learning/OSL_NLP/citeceer.mln)
  * Training files for online learning: [micro-batches](/Examples/Structure_Learning/OSL_NLP/training/)

Parameters:
 * Non-evidence predicates: `InField/3`
 * Max clause length: `12`
 * AdaGrad learning rate: `0.001`
 * Evaluation threshold: `2`

***OSL Learning***

```lang-none
lomrf-slearn -i empty.mln -t ./training/ -o learned.mln -m citeceer.modes -ne InField/3 -maxLength 12 -lambda 0.001 -threshold 2
```

## OSLa in Activity Recognition

In this example we demonstrate how to perform structure learning for activity recognition, using a fragment of the first
set of the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/). We use the same Probabilistic Event Calculus
formalism as presented in the [Quick Start](0_quick_start.md) section and an empty knowledge base having only the Probabilistic
Event Calculus axioms.

### Knowledge base (empty.mln)

We begin from an empty knowledge base that specify only the predicate and function schema, as well as the Probabilistic
Event Calculus axioms. We present below the initial knowledge base for the `meet` fluent.

```lang-none
// ----------------------------------------------------------------------------
// --- Domain definitions
//
// time: the domain of time-points
// fluent: the domain of fluent, that is composite events (CE)
// event: the domain of events, that is the input simple, derived events (SDE)
// id: domain entities, e.g. persons or objects
// ----------------------------------------------------------------------------

// Query Predicates:
HoldsAt(fluent, time)

// Template Predicates:
InitiatedAt(fluent, time)
TerminatedAt(fluent, time)

// Evidence Predicates (closed-world assumption):
Happens(event, time)
Close(id, id, distance, time)
OrientationMove(id, id, time)
Next(time, time)
StartTime(time)

// Function definitions for simple, derived events (SDE):
event enter(id)
event exit(id)
event walking(id)
event running(id)
event active(id)
event inactive(id)

// Function definitions for composite events (CE):
fluent meet(id, id)

// ----------------------------------------------------------------------------
// --- Probabilistic Event Calculus (domain-independent axioms)
// ----------------------------------------------------------------------------

// When a fluent holds:
Next(t1, t0) ^ InitiatedAt(f, t0) => HoldsAt(f, t1)

// When a fluent does not hold:
Next(t1, t0) ^ TerminatedAt(f, t0) => !HoldsAt(f, t1)

// The inertia of holdsAt, i.e. a fluent continues to hold unless it is terminated:
Next(t1, t0) ^ HoldsAt(f, t0) ^ !TerminatedAt(f, t0) => HoldsAt(f, t1).

// The inertia of !holdsAt, i.e. a fluent continues not to hold unless it is initiated:
Next(t1, t0) ^ !HoldsAt(f, t0) ^ !InitiatedAt(f, t0) => !HoldsAt(f, t1).

// ----------------------------------------------------------------------------
// --- Other domain-dependent rules
// ----------------------------------------------------------------------------

// --- The meeting CE cannot performed by a single person:
!HoldsAt(meet(p, p), t).

// --- Initially nothing holds
StartTime(t) => !HoldsAt(f, t).
```

### Mode Declarations (citeceer.modes)

Similar to OSL, we define a set of mode declarations, a form of language bias, to constrain the
structure of clauses learned. In this case we also have mode declarations for the functions.

*Mode declarations:*
```lang-none
// Modes declarations for predicates
modeP(2, Happens(-,+))
modeP(0, HoldsAt(+,+))
modeP(0, Next(-,+))
modeP(0, OrientationMove(.,.,+))
modeP(0, StartTime(+))
modeP(1, Close(.,.,#.,+))

// Mode declarations for functions
modeF(2, inactive(.))
modeF(2, walking(.))
modeF(2, active(.))
modeF(0, running(.))
modeF(1, meet(.,.))
modeF(2, enter(.))
modeF(2, exit(.))
```

###Training data

The training micro-batches are composed of ground facts of the input predicates `StartTime/1`, `Happens/2`, `Close/4`, `OrientationMove/3`, `Next/2`
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

### Weight Learning

The files of this example are the following:
  * Knowledge base files:
    * Main MLN file in CNF: [theory_cnf.mln](/Examples/Weight_Learning/Activity_Recognition/theory.mln)
    * Definitions of moving activity: [definitions/moving.mln](/Examples/Weight_Learning/Activity_Recognition/definitions/moving.mln)
    * Definitions of meeting activity: [definitions/meeting.mln](/Examples/Weight_Learning/Activity_Recognition/definitions/meeting.mln)
  * Training file for batch learning: [training.db](/Examples/Weight_Learning/Activity_Recognition/training/batch/training.db)
  * Training files for online learning: [micro-batches](/Examples/Weight_Learning/Activity_Recognition/training/online/)


### Structure Learning

The files of this example are the following:
  * Initially empty MLN file: [meet.mln](/Examples/Structure_Learning/OSLa_CAVIAR/meet.mln)
  * Mode declaration file: [meet.modes](/Examples/Structure_Learning/OSLa_CAVIAR/meet.modes)
  * Training files for online learning: [micro-batches](/Examples/Structure_Learning/OSLa_CAVIAR/training/meet)

Parameters:
 * Non-evidence predicates: `HoldsAt/2`
 * Template predicates: `InitiatedAt/2`, `TerminatedAt/2`
 * Max clause length: `8`
 * Evaluation threshold: `1`

***OSLa Learning***

```lang-none
lomrf-slearn -i meet.mln -o learned_meet.mln -t ./training/meet -m meet.modes -ne HoldsAt/2 -template InitiatedAt/2,TerminatedAt/2 -maxLength 8 -threshold 1
```