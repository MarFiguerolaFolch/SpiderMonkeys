# Statistical analysis of Visual Discrimination and Long-term Memory in Black-handed Spider-Monkeys
This repository contains R scripts used for the statistical analysis of data collected for my Master's thesis in Applied Ethology and Animal Biology.
The study investigated visual discrimination of three-dimensional and visual long-term memory in black-handed spider monkeys (Ateles geoffroyi). 
Nine spider monkeys were tested in a two-choice operant conditioning paradigm using 12 three-dimensional objects arranged into 25 stimulus pairs. Visual long-term memory was assessed using 15 previously learned stimulus pairs after retention intervals of 4, 6, 8, 10, and 12 weeks respectively.

Experiment 1 - Visual discrimination
This first experiment investigated the ability of spider monkeys to visually discriminate between three-dimensional objects differing in a single visual feature:
- Dimension
- Spatial orientation
- Geometry

All nine individuals successfully learned to discriminate all 25 stimulus pairs, and showed a progressive decrease in the number of sessions required to reach the learning criterion across the task.

An exploratory analysis found no systematic differences in learning speed nor discrimination performance between stimulus categories.

Experiment 2 - Visual long-term memory
The second experiment assessed memory for previously learned visual stimulus-reward associations after the retention intervals.
Spider monkeys showed no decline in memory performance even at the longest retention interval tested.

Analysis scripts
1. Visual discrimination (01_visual_discrimination.R)
Analyses visual discrimination performance and the number of sessions required to reach the learning criterion.

2. Long-term memory: first-trial after retention interval (02_memory_first_trial.R)
Analyses first-trial performance after the retention intervals.

3. Individual differences (03_individual_differences_learning_speed.R)
Analyses interindividual differences in learning speed using a linear mixed-effects model fitted with restricted maximum likelihood estimation. Interindividual variation in test performance can arise from factors such as age, sex, sensory capacities, and motivation.  

The original Excel datasets are not included in this repository.

The findings from this research were published in Animal Cognition:
Visual discrimination of three-dimensional objects and visual long-term memory in spider monkeys (Ateles geoffroyi). 
https://doi.org/10.1007/s10071-026-02126-5 
