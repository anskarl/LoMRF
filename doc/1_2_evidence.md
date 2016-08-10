# Evidence

Evidence files in LoMRF are text files having the suffix `.db` (e.g., `file_name.db`). The contents of an evidence file
are ground predicates (facts) and optionally ground function Mappings.

## Function Mappings

A function mapping defines a possible **true** grounding of a function (see [Function Definitions](1_1_knowledge_base.md#function-definitions)).
Syntactically follows the corresponding function definition in the knowledge base file, but the domain types are
replaced with some of their corresponding constant symbols.  

For example, the *true* possible groundings of the function `time = next(time)` are the following:

```lang-none
2 = next(1)
3 = next(2)
4 = next(3)
5 = next(4)
6 = next(5)
7 = next(6)
8 = next(7)
9 = next(8)
10 = next(9)
```

According to the given true groundings in the example above, `next(1)` results to the constant `2`. Furthermore,
LoMRF takes a [Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption) approach for function
mappings and therefore mappings that are not defined in the evidence are implicitly assumed as **false**. For example,
the mapping of `next(0)` is missing from the above evidence, thus it is implicitly assumed as **false**.

## Facts (ground predicates)

Ground predicates in the evidence represent known facts for the LoMRF. Each fact is expressed with predicates that contain
only constants from their corresponding domains. Each fact represents a true grounding of a specific predicate, optionally
facts can be negated and thus represent a false grounding of a predicate.

For example, the following ground predicates are facts that express that *Agamemnon* and Menelaus are brothers,
but *Achilles* is not brother of *Agamemnon*.

```lang-none
Brother(Agamemnon, Menelaus)
Brother(Menelaus, Agamemnon)
!Brother(Achilles, Agamemnon)
```

By default LoMRF takes [Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption) for all
instantiations of predicates that have at least one fact in the specified evidence file, unless it is explicitly
specified to take [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) in the inference
options or it is a query predicate (see [Inference](2_inference.md)). Therefore, we can define only the true
groundings in the evidence and assume a *False* state for all other possible combinations (e.g., `Brother(Achilles, Menelaus)` is implicitly assumed as **False**).
