# FaceWord Project -- Data Analysis

## Experiment designs
This file displays the results of the FaceWord project (data collected at NYU). There are two experiments in this project. In Experiment 1, Chinese participants viewed Chinese faces and characters in four conditions (*Layout*: intact, exchange [top and bottom parts were switched], top and bottom) and completed an additional localizer (Chinese faces, Chinese characters, objects, scrambled objects). In Experiment 2, English speakers viewed Chinese characters and English words in four conditions (*Layout*: intact, exchange, top [top parts of Chinese characters; left two letters for English words] and bottom [bottom parts of Chinese characters; right four letters for English words]) and completed an additional localizer (Caucasian faces, English words, objects, scrambled objects).

## Introduction to the analyses included in this file
For the **main runs**, analysis is conducted for each ROI separately (FFA1, FFA2, VWFA, LOC).   
For each ROI, three analyses are performed: 

1. Univariate analysis (Repeated-measures ANOVA) is performed to compare the neural responses (beta values) of different conditions.
    + E1: 2(Chinese faces vs. Chinese Characters) * 4 (intact, exchange, top vs. bottom); 
    + E2: 2(Chinese characters vs. English words) * 4 (intact, exchange, top vs. bottom).
2. Multivariate pattern analysis (MVPA) with `libsvm` is used to decode different condition pairs (see below) and one-tail one-sample t-tests is used to test if the pair of conditions can be decoded [whether the accuracy is significantly larger than the chancel level (0.5); one-tail one-sample t-tests]. Leave-one(-run)-out cross-validation is applied. No normalized or demean were used.
    + Pairs in E1: 
        + face_intact vs. word_intact;
        + face_intact vs. face_exchange;
        + face_top vs. face_bottom;
        + word_intact vs. word_exchange;
        + word_top vs. word_bottom.
    + Pairs in E2:
        + English_intact vs. Chinese_intact;
        + English_intact vs. English_exchange;
        + English_left vs. English_right;
        + Chinese_intact vs. Chinese_exchange;
        + Chinese_top vs. Chinese_bottom.
3. Similarity of top+bottom to intact vs. exchange: The dependent variable is the probability of top+bottom was decoded as Exchange conditions. Two-tail one-sample t-tests is used to test if top+bottom is more similar to exchange relative to intact.
    + If the pattern of top+bottom is more similar to that of exchange relative to intact, the probability (of being decoded as exchange) should be *significantly larger* than the chance level (0.5).
    + If the pattern of top+bottom is more similar to that of intact relative to exchange, the probability (of being decoded as exchange) should be *significantly smaller* than the chance level (0.5).
    
## How the labels are defined for each ROI?
1. Identify the vertex whose beta value is larger than the surrounding vertices (i.e., the local maxima) for each ROI based on the reference coordinates in previous literature.
2. Dilate the region centering at the local maxima and only keep 50% of the "peripheral" vertices whose response were larger. This step is iterated until the size of the ROI reaches the pre-defined size (100mm^2), during which the vertices are masked by a pre-defined label at the threshold of p < .05. In other words, the p-values for all vertices in the labels are smaller than .05 (uncorrected).

## How is the probability of top+bottom being decoded as exchange calculated?
The probability was estimated for each particiapnt separately:

1. The patterns of top and bottom are combined with three different weights (0.5/0.5, 0.25/0.75, 0.75/0.25).
2. Supported Vector Machine (`libsvm`) is trained with the patterns of intact vs. exchange (10 runs).
3. The trained model is used to predict the probability of the combined patterns being decoded as exchange [for each run separately]. 
4. The probability of top+bottom being decoded as exchange for each participant is calculated by averaging the probability for each run.
