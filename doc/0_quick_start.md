# Quick Start

Assuming that you have successfully build a LoMRF distribution and added LoMRF executables in your default `PATH` (see 
[Building and Linking](5_building_and_linking.md)). In the following paragraphs we will write our first LoMRF model and
perform probabilistic inference.

For our first example we will use the [Yale Shooting Problem](https://en.wikipedia.org/wiki/Yale_shooting_problem), 
a well-known test case of non-monotonic temporal reasoning which is proposed by Steve Hanks and Drew McDermott[^1]. 
In brief, a hunter tries to shoot a turkey with his gun. In order to shoot the turkey, the gun must be loaded. When 
the hunter shoots with a loaded gun the turkey is being killed. 


In this example, we model the Yale Shooting scenario by using [Event Calculus](https://en.wikipedia.org/wiki/Event_calculus) 
logical action formalism. 

In the example scenario there is a sequence of *actions* (e.g., loading the gun) and *situations* (e.g., the turkey is 
alive). The series of actions and situations occur in time. For example, at each instant of time some situation holds 
(e.g., the gun is loaded) or an event may happen (e.g., load the gun). In the Event Calculus we represent *actions* with 
*events* and *situations* with *fluents*. More formally, a fluent is a property whose value may change over time. When 
an event occurs it may change the value of a fluent. The underlying time  model is linear and we represent time-points 
as integer numbers. The core domain-independent axioms of the Event Calculus define whether a fluent holds or not at a 
specific time-point. Moreover, the axioms incorporate the common sense *law of inertia*, according to which fluents 
persist over time, unless they are affected by the occurrence of some event. For example, the gun remains loaded until 
the hunter shoots.

Below we outline the events and the fluents that we will use in the example: 

| Event | Description                |
|:------|:---------------------------|
| Shoot | Gun is shoot at the turkey |
| Load  | Load the gun               |

| Fluent | Description               |
|:-------|:--------------------------|
| Loaded | The gun is loaded         |
| Alive  | The turkey is alive       |
| Dead   | The turkey is dead        |

  
  
The turkey is initially alive and the hunter's gun is initially unloaded. By shooting with unloaded gun the state of the 
turkey remains the same, that is alive.

Loading the gun, waiting for a moment, and then shooting 
the gun at the turkey is expected to kill the turkey. 

However, if inertia is formalized in logic by minimizing the changes in this situation, then it cannot be uniquely 
proved that Fred is dead after loading, waiting, and shooting. In one solution, Fred indeed dies; in another (also logically correct) 
solution, the gun becomes mysteriously unloaded and Fred survives.

## Writing your first knowledge base

## Writing your first evidence

## Perform inference


## References

[^1]: S. Hanks and D. McDermott. 1987. Nonmonotonic logic and temporal projection. Artificial Intelligence 33.3, 379-412.