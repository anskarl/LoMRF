# Contributing to LoMRF

Do you like LoMRF and want to get involved? Cool! That is wonderful!

Please take a moment to review this document, in order to make the contribution process easy and effective for everyone 
involved.

## Core Ideas

The purpose of LoMRF is to provide both a usable tool and a library for creating models using the Statistical Relational 
Learning concepts of Markov Logic Networks (MLNs). As a tool we aim to have a great end-to-end experience of MLNs 
modelling, inference and Machine Learning. This means that we prefer to have features that are mature enough to be part 
of the LoMRF. We prefer to postpone the release of a feature, in order to have implementations that are clean in terms 
of user experience and development friendliness, well-documented (documentation and examples) and well-tested (unit 
tests, example code). LoMRF is composed of a collection of complex algorithms and always we would like to have efficient 
implementations. As a result, another important aspect of LoMRF is to have fast and memory-friendly implementations of 
the core algorithms (e.g., inference).

There are two main branches, master and develop. The former, contains the stable versions of LoMRF, thus it is not related 
to active development version, pull requests and hot-fixes. The code in master branch is considered as frozen. Even in 
situations of hot-fixes or minor improvements we prefer to fix them in the development version first. The latter, 
develop branch, contains the latest development snapshot of LoMRF. We strongly suggest to work your contributions over 
the develop branch.

## Submitting a Pull Request

Good pull requests, such as patches, improvements, and new features, are a fantastic help. They should remain focused 
in scope and avoid containing unrelated commits.

Please **ask first** if somebody else is already working on this or the core developers think your feature is in-scope 
for LoMRF. Generally always have a related issue with discussions for whatever you are including.

Please also provide a test plan, i.e., specify how you verified that your addition works, add unit tests or provide 
examples.

Finally, since master branch is only for stable releases tagged with a version, **a pull request should be always target 
to the develop branch.**


Thank you again for considering to contribute to LoMRF and happy hacking :)