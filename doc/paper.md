---
title: 'DAG: A Fortran package for directed acyclic graphs'
tags:
  - Fortran
  - directed acyclic graph
  - parallel
  - object-oriented programming
authors:
  - name: Damian Rouson^[Corresponding author.]
    orcid: 0000-0002-2344-868X
    affiliation: 1
  - name: Robert Singleterry
    orcid: 0000-0002-5725-8825
    affiliation: 2
  - name: Brad Richardson
    orcid: 0000-0002-3205-2169
    affiliation: 3
affiliations:
 - name: Lawrence Berkeley National Laboratory and Archaeologic Inc.
   index: 1
 - name: NASA Langley Research Center
   index: 2
 - name: Archaeologic Inc.
   index: 3
date: 5 September 2021
bibliography: paper.bib

---

# Summary

A directed acyclic graph (DAG) comprises vertices connected by edges along which
no cyclic paths exist.  DAG resulted from a refactoring and updating
[DAGLIB](https://github.com/jacobwilliams/daglib) to leverage the increased
modularity, explicit parallelism, purely functional capabilities of Fortran
2018.

# Statement of need

Our primary interest lies in parallel task scheduling for the On-Line Tool for
Assessment of Radiation in Space (OLTARIS, Figure \autoref{fig:oltaris})
[@singleterry2011oltaris].  We are publishing both the task scheduling framework
and DAG as open-source to make them available to the broader Fortran community
for use in any applications that require a DAG abstraction.

![OLTARIS web site.\label{fig:oltaris}](OLTARIS.png)


# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.

# References
