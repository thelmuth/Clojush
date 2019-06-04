#!/usr/bin/python

## Can take 0, 1, or 2 command line arguments.
## If 0 arguments, uses variable set to outputDirectory as the location of the output files.
## First argument is the location of the output files, and overrides a variable defined below.
## Second argument can be "brief" in order to not output individual run success/fail, and only output aggregate statistics

import os, sys
from sys import maxint

# Set these before running:

verbose = True
if (len(sys.argv) >= 2 and sys.argv[1] == "brief") or \
        (len(sys.argv) >= 3 and sys.argv[2] == "brief"):
    verbose = False

##########################################################################################
########################### Parent Selection Experiments v2 ##############################
##########################################################################################

#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/syllables/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/double-letters/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/string-lengths-backwards/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/x-word-lines/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/number-io/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/small-or-large/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/for-loop-index/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/compare-string-lengths/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/collatz-numbers/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/string-differences/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/even-squares/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/wallis-pi/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/last-index-of-zero/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/vector-average/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/count-odds/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/mirror-image/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/super-anagrams/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/sum-of-squares/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/negative-to-zero/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/vectors-summed/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/median/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/smallest/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/digits/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/pig-latin/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/scrabble-score/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/word-stats/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/grade/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase/checksum/"

#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/lexicase-3000-gens/string-differences/"

#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/syllables/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/vector-average/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/mirror-image/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/number-io/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/smallest/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/last-index-of-zero/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/double-letters/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/compare-string-lengths/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/scrabble-score/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/x-word-lines/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/negative-to-zero/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/count-odds/"
#outputDirectory = "/home/thelmuth/Results/parent-selection-v2/tournament/sum-of-squares/"



#######################################################################################
#### Sarah Troise's Runs: Weighted Lexicase Selection
#######################################################################################

#outputDirectory = "/home/stroise/results/weighted-lexicase/number-of-zeros/replace-space-with-newline/"
#outputDirectory = "/home/stroise/results/weighted-lexicase/number-of-zeros/syllables/"

#outputDirectory = "/home/stroise/results/weighted-lexicase/number-of-zeros-inverse/replace-space-with-newline/"
#outputDirectory = "/home/stroise/results/weighted-lexicase/number-of-zeros-inverse/syllables/"

#outputDirectory = "/home/stroise/results/weighted-lexicase/number-of-nonzero/replace-space-with-newline/"
#outputDirectory = "/home/stroise/results/weighted-lexicase/number-of-nonzero/syllables/"
#outputDirectory = "/home/stroise/results/weighted-lexicase/number-of-nonzero/scrabble-score/"

#outputDirectory = "/home/stroise/results/weighted-lexicase/number-of-nonzero-inverse/replace-space-with-newline/"

#outputDirectory = "/home/stroise/results/weighted-lexicase/median/replace-space-with-newline/"

#outputDirectory = "/home/stroise/results/weighted-lexicase/variance/replace-space-with-newline/"
#outputDirectory = "/home/stroise/results/weighted-lexicase/variance/syllables/"

####

#outputDirectory = "/home/stroise/results/ranked-lexicase/number-of-nonzero/replace-space-with-newline/"
#outputDirectory = "/home/stroise/results/ranked-lexicase/number-of-nonzero/syllables/"

#outputDirectory = "/home/stroise/results/bias-lexicase/number-of-nonzero-inverse/replace-space-with-newline/"
#outputDirectory = "/home/stroise/results/bias-lexicase/number-of-nonzero-inverse/syllables/"

#outputDirectory = "/home/stroise/results/bias-lexicase/average/replace-space-with-newline/"

################# My runs TMH

#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/weighted-lexicase/median-inverse/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/weighted-lexicase/average/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/weighted-lexicase/variance-inverse/replace-space-with-newline/"

#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/weighted-lexicase/median-inverse/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/weighted-lexicase/average/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/weighted-lexicase/variance-inverse/syllables/"

## Ranked Lexicase

#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/number-of-zeros/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/number-of-zeros-inverse/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/median/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/median-inverse/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/number-of-nonzero/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/number-of-nonzero-inverse/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/variance/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/variance-inverse/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/average/replace-space-with-newline/"

#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/number-of-zeros/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/number-of-zeros-inverse/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/median/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/median-inverse/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/number-of-nonzero/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/number-of-nonzero-inverse/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/variance/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/variance-inverse/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/ranked-lexicase/average/syllables/"

## Bias Lexicase TMH

#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/number-of-zeros/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/number-of-zeros-inverse/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/median/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/median-inverse/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/number-of-nonzero/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/number-of-nonzero-inverse/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/variance/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/variance-inverse/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/average/replace-space-with-newline/"

#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/number-of-zeros/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/number-of-zeros-inverse/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/median/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/median-inverse/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/number-of-nonzero/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/number-of-nonzero-inverse/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/variance/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/variance-inverse/syllables/"
#outputDirectory = "/home/thelmuth/Results/weighted-lexicase/bias-lexicase/average/syllables/"


#######################################################################################
#### Divide and Conquer Lexicase
#######################################################################################

#outputDirectory = "Results/parent-selection-v2/divide-and-conquer-lexicase/group-size-2/replace-space-with-newline/"
#outputDirectory = "Results/parent-selection-v2/divide-and-conquer-lexicase/group-size-2/syllables/"
#outputDirectory = "Results/parent-selection-v2/divide-and-conquer-lexicase/group-size-3/replace-space-with-newline/"
#outputDirectory = "Results/parent-selection-v2/divide-and-conquer-lexicase/group-size-3/syllables/"
#outputDirectory = "Results/parent-selection-v2/divide-and-conquer-lexicase/group-size-5/replace-space-with-newline/"
#outputDirectory = "Results/parent-selection-v2/divide-and-conquer-lexicase/group-size-5/syllables/"


#######################################################################################
#### SMAC Parameters
#######################################################################################
#outputDirectory = "/home/thelmuth/Results/SMAC-params/v1/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/SMAC-params/v1/syllables/"
#outputDirectory = "/home/thelmuth/Results/SMAC-params/v1/string-lengths-backwards/"
#outputDirectory = "/home/thelmuth/Results/SMAC-params/v1/double-letters/"
#outputDirectory = "/home/thelmuth/Results/SMAC-params/v1/x-word-lines/"

#######################################################################################
#### Paired-Lexicase
#######################################################################################

#outputDirectory = "Results/renewed-parent-selection-tests/paired-lexicase/tuple-size-2/replace-space-with-newline/"
#outputDirectory = "Results/renewed-parent-selection-tests/paired-lexicase/tuple-size-3/replace-space-with-newline/"

#outputDirectory = "Results/renewed-parent-selection-tests/paired-lexicase/tuple-size-2/syllables/"
#outputDirectory = "Results/renewed-parent-selection-tests/paired-lexicase/tuple-size-3/syllables/"

#######################################################################################
#### Interleaved Sampling
#######################################################################################

#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/replace-space-with-newline/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/syllables/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/double-letters/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/string-lengths-backwards/"

#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/x-word-lines/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/count-odds/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/mirror-image/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/scrabble-score/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/vector-average/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/number-io/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/smallest/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/median/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/digits/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/checksum/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/super-anagrams/"
#outputDirectory = "Results/parent-selection-v2/interleaved-sampling/last-index-of-zero/"



#######################################################################################
#### Eliteness-Based Tournament
#######################################################################################

#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-2/replace-space-with-newline/"
#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-4/replace-space-with-newline/"
#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-7/replace-space-with-newline/"

#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-2/syllables/"
#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-4/syllables/"
#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-7/syllables/"

#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-2/string-lengths-backwards/"
#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-4/string-lengths-backwards/"
#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-7/string-lengths-backwards/"

#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-2/double-letters/"
#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-4/double-letters/"
#outputDirectory = "Results/parent-selection-v2/eliteness-based-tournament/size-7/double-letters/"

###################
## A few disparate tests

#outputDirectory = "Results/autoconstruction/autoconstruction-False/generations-2000/replace-space-with-newline/"


####################################################
## Epsilon Lexicase - program synthesis experiment

#outputDirectory = "Results/epsilon-lexicase/not-epsilon-lexicase/sextic/"

#outputDirectory = "Results/epsilon-lexicase/dynamic/sextic/"
#outputDirectory = "Results/epsilon-lexicase/dynamic/number-io/"
#outputDirectory = "Results/epsilon-lexicase/dynamic/vector-average/"
#outputDirectory = "Results/epsilon-lexicase/dynamic/wallis-pi/"
#outputDirectory = "Results/epsilon-lexicase/dynamic/double-letters/"
#outputDirectory = "Results/epsilon-lexicase/dynamic/x-word-lines/"
#outputDirectory = "Results/epsilon-lexicase/dynamic/count-odds/"
#outputDirectory = "Results/epsilon-lexicase/dynamic/replace-space-with-newline/"

#outputDirectory = "Results/epsilon-lexicase/static/sextic/"
#outputDirectory = "Results/epsilon-lexicase/static/number-io/"
#outputDirectory = "Results/epsilon-lexicase/static/vector-average/"
#outputDirectory = "Results/epsilon-lexicase/static/wallis-pi/"
#outputDirectory = "Results/epsilon-lexicase/static/double-letters/"
#outputDirectory = "Results/epsilon-lexicase/static/x-word-lines/"
#outputDirectory = "Results/epsilon-lexicase/static/count-odds/"
#outputDirectory = "Results/epsilon-lexicase/static/replace-space-with-newline/"

#outputDirectory = "Results/epsilon-lexicase/super-dynamic/sextic/"
#outputDirectory = "Results/epsilon-lexicase/super-dynamic/number-io/"
#outputDirectory = "Results/epsilon-lexicase/super-dynamic/vector-average/"
#outputDirectory = "Results/epsilon-lexicase/super-dynamic/wallis-pi/"
#outputDirectory = "Results/epsilon-lexicase/super-dynamic/double-letters/"
#outputDirectory = "Results/epsilon-lexicase/super-dynamic/x-word-lines/"
#outputDirectory = "Results/epsilon-lexicase/super-dynamic/count-odds/"
#outputDirectory = "Results/epsilon-lexicase/super-dynamic/replace-space-with-newline/"


#######################################################################################
#### Novelty Search
#######################################################################################

## Hamming

#outputDirectory = "Results/parent-selection-v2/novelty-search/hamming/replace-space-with-newline/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/hamming/mirror-image/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/hamming/smallest/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/hamming/compare-string-lengths/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/hamming/super-anagrams/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/hamming/number-io/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/hamming/median/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/hamming/string-lengths-backwards/"


## Manhattan

#outputDirectory = "Results/parent-selection-v2/novelty-search/manhattan/last-index-of-zero/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/manhattan/count-odds/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/manhattan/vector-average/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/manhattan/string-lengths-backwards/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/manhattan/replace-space-with-newline/"
#outputDirectory = "Results/parent-selection-v2/novelty-search/manhattan/number-io/"


#######################################################################################
## Madu
#######################################################################################

#outputDirectory = "Results/madu/try1/sphere/"
#outputDirectory = "Results/madu/try1/bouncing-ball/"

#outputDirectory = "Results/madu/try2/sphere/"
#outputDirectory = "Results/madu/try2/bouncing-ball/"


#######################################################################################
## Tag Epigenetics
#######################################################################################

#outputDirectory = "Results/epigenetic-tags/only-tag-blocks/replace-space-with-newline/"


#######################################################################################
## Decay (Entropy) Addition/Deletion
#######################################################################################

# Entropy (first try)
# These runs had a bunch of mysterious garbage collection crashes. Reruns below.

#outputDirectory = "Results/entropy-add-delete/add-delete-only/replace-space-with-newline/"
#outputDirectory = "Results/entropy-add-delete/add-delete-only/string-differences/"

#outputDirectory = "Results/entropy-add-delete/typical-plus-deletion/replace-space-with-newline/"
#outputDirectory = "Results/entropy-add-delete/typical-plus-deletion/string-differences/"
#### A SUCCESS ON S-D in the runs above! I moved the file to SUCCESS-log9.txt to restart the others.

#outputDirectory = "Results/entropy-add-delete/replace-mut-with-add/replace-space-with-newline/"
#outputDirectory = "Results/entropy-add-delete/replace-mut-with-add/string-differences/"

################
########## Decay (second try)
################

### Add Delete Only
#outputDirectory = "Results/decay-add-delete/add-delete-only/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/syllables/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/count-odds/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/x-word-lines/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/string-differences/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/last-index-of-zero/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/vector-average/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/mirror-image/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/super-anagrams/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/sum-of-squares/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/scrabble-score/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/checksum/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/digits/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/grade/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/median/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/smallest/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/number-io/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/small-or-large/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/pig-latin/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/for-loop-index/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/compare-string-lengths/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/double-letters/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/string-lengths-backwards/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/vectors-summed/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/negative-to-zero/"
#outputDirectory = "Results/decay-add-delete/add-delete-only/even-squares/"
#outputDirectory = "Results/decay-add-delete/add-delete-only//"

## For EDNs
#outputDirectory = "Results/decay-add-delete/EDN-printing/add-delete-only/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/EDN-printing/add-delete-only/syllables/"


### Typical Plus Deletion
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/syllables/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/count-odds/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/x-word-lines/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/string-differences/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/last-index-of-zero/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/vector-average/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/mirror-image/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/super-anagrams/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/sum-of-squares/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/scrabble-score/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/checksum/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/digits/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/grade/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/median/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/smallest/"
#outputDirectory = "Results/decay-add-delete/typical-plus-deletion/even-squares/"


### Replace mutation with addition
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/syllables/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/count-odds/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/x-word-lines/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/string-differences/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/last-index-of-zero/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/vector-average/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/mirror-image/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/super-anagrams/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/sum-of-squares/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/scrabble-score/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/checksum/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/digits/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/grade/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/median/"
#outputDirectory = "Results/decay-add-delete/replace-mut-with-add/smallest/"


### Size-Neutral Add+Delete
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/syllables/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/vector-average/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/compare-string-lengths/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/last-index-of-zero/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/mirror-image/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/double-letters/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/x-word-lines/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/sum-of-squares/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/negative-to-zero/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/scrabble-score/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/checksum/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/string-lengths-backwards/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/vectors-summed/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/count-odds/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/median/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/digits/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/grade/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/number-io/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/small-or-large/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/for-loop-index/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/even-squares/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/super-anagrams/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/pig-latin/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete/smallest/"

### Size-Growing Add+Delete
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/syllables/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/vector-average/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/compare-string-lengths/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/last-index-of-zero/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/mirror-image/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/double-letters/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/x-word-lines/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/negative-to-zero/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/checksum/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/compare-string-lengths/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/count-odds/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/digits/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/grade/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/median/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/scrabble-score/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/string-lengths-backwards/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/sum-of-squares/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/vectors-summed/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/number-io/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/small-or-large/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/for-loop-index/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/even-squares/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/super-anagrams/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/pig-latin/"
#outputDirectory = "Results/decay-add-delete/size-growing-add-delete/smallest/"



### 50-50 Add+Delete and Alternation
#outputDirectory = "Results/decay-add-delete/add-delete-alternation-50-50/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/add-delete-alternation-50-50/syllables/"
#outputDirectory = "Results/decay-add-delete/add-delete-alternation-50-50/vector-average/"

### 50-50 Add+Delete and Alternation (size-neutral)
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-alternation-50-50/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-alternation-50-50/syllables/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-alternation-50-50/vector-average/"


### Half Addition, Half Deletion
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/syllables/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/vector-average/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/double-letters/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/last-index-of-zero/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/x-word-lines/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/sum-of-squares/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/negative-to-zero/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/scrabble-score/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/count-odds/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/mirror-image"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/checksum/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/compare-string-lengths/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/count-odds/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/digits/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/grade/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/median/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/string-lengths-backwards/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/vectors-summed/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/number-io/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/small-or-large/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/for-loop-index/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/even-squares/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/super-anagrams/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/pig-latin/"
#outputDirectory = "Results/decay-add-delete/half-add-half-delete/smallest/"



### Smaller and larger Populations (size shrinking add-delete)
#outputDirectory = "Results/decay-add-delete/pop-size-100/add-delete-only/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/pop-size-100/add-delete-only/syllables/"
#outputDirectory = "Results/decay-add-delete/pop-size-100/add-delete-only/vector-average/"

#outputDirectory = "Results/decay-add-delete/pop-size-10/add-delete-only/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/pop-size-10/add-delete-only/syllables/"
#outputDirectory = "Results/decay-add-delete/pop-size-10/add-delete-only/vector-average/"

#outputDirectory = "Results/decay-add-delete/pop-size-10000/add-delete-only/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/pop-size-10000/add-delete-only/syllables/"
#outputDirectory = "Results/decay-add-delete/pop-size-10000/add-delete-only/vector-average/"

### Smaller and larger Populations (size-neutral add-delete)
#outputDirectory = "Results/decay-add-delete/pop-size-100/size-neutral-add-delete/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/pop-size-100/size-neutral-add-delete/syllables/"
#outputDirectory = "Results/decay-add-delete/pop-size-100/size-neutral-add-delete/vector-average/"

#outputDirectory = "Results/decay-add-delete/pop-size-10/size-neutral-add-delete/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/pop-size-10/size-neutral-add-delete/syllables/"
#outputDirectory = "Results/decay-add-delete/pop-size-10/size-neutral-add-delete/vector-average/"

#outputDirectory = "Results/decay-add-delete/pop-size-10000/size-neutral-add-delete/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/pop-size-10000/size-neutral-add-delete/syllables/"
#outputDirectory = "Results/decay-add-delete/pop-size-10000/size-neutral-add-delete/vector-average/"


### Add Delete Only with different rates
#outputDirectory = "Results/decay-add-delete/add-delete-rate-018/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/add-delete-rate-018/syllables/"
#outputDirectory = "Results/decay-add-delete/add-delete-rate-018/vector-average/"

#outputDirectory = "Results/decay-add-delete/add-delete-rate-0045/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/add-delete-rate-0045/syllables/"
#outputDirectory = "Results/decay-add-delete/add-delete-rate-0045/vector-average/"


### Size-Neutral Add Delete Only with different rates
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-018/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-018/syllables/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-018/vector-average/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-018/mirror-image/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-018/double-letters/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-018/x-word-lines/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-018/sum-of-squares/"

#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-0045/replace-space-with-newline/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-0045/syllables/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-0045/vector-average/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-0045/mirror-image/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-0045/double-letters/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-0045/x-word-lines/"
#outputDirectory = "Results/decay-add-delete/size-neutral-add-delete-rate-0045/sum-of-squares/"


#######
### Uniform Combination and Deletion
######

#outputDirectory = "/home/thelmuth/Results/decay-add-delete/uniform-combination-and-deletion-1/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/decay-add-delete/uniform-combination-and-deletion-1/syllables/"
#outputDirectory = "/home/thelmuth/Results/decay-add-delete/uniform-combination-and-deletion-1/vector-average/"

#outputDirectory = "/home/thelmuth/Results/decay-add-delete/uniform-combination-and-deletion-009/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/decay-add-delete/uniform-combination-and-deletion-009/syllables/"
#outputDirectory = "/home/thelmuth/Results/decay-add-delete/uniform-combination-and-deletion-009/vector-average/"

#outputDirectory = "/home/thelmuth/Results/decay-add-delete/uniform-combination-009-followed-by-deletion-01/replace-space-with-newline/"
#outputDirectory = "/home/thelmuth/Results/decay-add-delete/uniform-combination-009-followed-by-deletion-01/syllables/"
#outputDirectory = "/home/thelmuth/Results/decay-add-delete/uniform-combination-009-followed-by-deletion-01/vector-average/"




########
### Mutation only with rate 0.09
########

#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/replace-space-with-newline/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/syllables/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/vector-average/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/sum-of-squares/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/x-word-lines/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/last-index-of-zero/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/scrabble-score/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/mirror-image/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/double-letters/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/count-odds/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/scrabble-score/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/compare-string-lengths/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/vectors-summed/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/negative-to-zero/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/string-lengths-backwards/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/checksum/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/digits/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/median/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/grade/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/number-io/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/small-or-large/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/for-loop-index/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/even-squares/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/super-anagrams/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/pig-latin/"
#outputDirectory = "Results/parent-selection-v2/mutation-only-rate-09/smallest/"



############################
### Tournament Selection
### Size-Neutral Add+Delete
############################

#outputDirectory = "Results/decay-add-delete/tournament/size-neutral-add-delete/number-io/"
#outputDirectory = "Results/decay-add-delete/tournament/size-neutral-add-delete/smallest/"
#outputDirectory = "Results/decay-add-delete/tournament/size-neutral-add-delete/mirror-image/"




####################################################################################################
###### Hyperselection experiments: semantic SLT (sampled-lexicase-tournament selection)
####################################################################################################

#outputDirectory = "Results/sampled-lexicase-tournament/semantic-SLT-by-samples/replace-space-with-newline/"
#outputDirectory = "Results/sampled-lexicase-tournament/semantic-SLT-by-samples/syllables/"
#outputDirectory = "Results/sampled-lexicase-tournament/semantic-SLT-by-samples/vector-average/"
#outputDirectory = "Results/sampled-lexicase-tournament/semantic-SLT-by-samples/mirror-image/"
#outputDirectory = "Results/sampled-lexicase-tournament/semantic-SLT-by-samples/negative-to-zero/"
#outputDirectory = "Results/sampled-lexicase-tournament/semantic-SLT-by-samples/x-word-lines/"
#outputDirectory = "Results/sampled-lexicase-tournament/semantic-SLT-by-samples/string-lengths-backwards/"

### Parent selection v2: tournament

#outputDirectory = "Results/parent-selection-v2/tournament/replace-space-with-newline/"
#outputDirectory = "Results/parent-selection-v2/tournament/syllables/"
#outputDirectory = "Results/parent-selection-v2/tournament/vector-average/"


############
### Alex Dennis's honors thesis experiments on DOF epsilon lexicase
############

#outputDirectory = "Results/DOF-lexicase/x-word-lines/"
#outputDirectory = "Results/DOF-lexicase/double-letters/"
#outputDirectory = "Results/DOF-lexicase/small-or-large/"
#outputDirectory = "Results/DOF-lexicase/digits/"
#outputDirectory = "Results/DOF-lexicase/for-loop-index/"


########################################################################################
##### Elitist Survival Selection - specialists for lexicase
########################################################################################

#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/replace-space-with-newline/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/syllables/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/string-lengths-backwards/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/mirror-image/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/last-index-of-zero/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/negative-to-zero/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/double-letters/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/vector-average/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/compare-string-lengths/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/sum-of-squares/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase/x-word-lines/"
#outputDirectory = "Results/elitist-survival-2018/rate-50/lexicase//"

#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/replace-space-with-newline/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/syllables/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/string-lengths-backwards/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/mirror-image/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/last-index-of-zero/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/negative-to-zero/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/double-letters/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/vector-average/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/compare-string-lengths/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/sum-of-squares/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase/x-word-lines/"
#outputDirectory = "Results/elitist-survival-2018/rate-20/lexicase//"

#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/replace-space-with-newline/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/syllables/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/string-lengths-backwards/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/mirror-image/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/last-index-of-zero/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/negative-to-zero/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/double-letters/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/vector-average/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/compare-string-lengths/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/sum-of-squares/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase/x-word-lines/"
#outputDirectory = "Results/elitist-survival-2018/rate-10/lexicase//"

########################################################################################
##### Plushi
########################################################################################

#outputDirectory = "Results/plushi/replace-space-with-newline/"
#outputDirectory = "Results/plushi/syllables/"
#outputDirectory = "Results/plushi/string-lengths-backwards/"
#outputDirectory = "Results/plushi/mirror-image/"
#outputDirectory = "Results/plushi/last-index-of-zero/"
#outputDirectory = "Results/plushi/negative-to-zero/"
#outputDirectory = "Results/plushi/double-letters/"
#outputDirectory = "Results/plushi/vector-average/"
#outputDirectory = "Results/plushi/compare-string-lengths/"
#outputDirectory = "Results/plushi/sum-of-squares/"
#outputDirectory = "Results/plushi/x-word-lines/"
#outputDirectory = "Results/plushi/wallis-pi/"
#outputDirectory = "Results/plushi/pig-latin/"
#outputDirectory = "Results/plushi/even-squares/"
#outputDirectory = "Results/plushi/checksum/"


#outputDirectory = "Results/wc-new-experiments/UMAD/wc"
outputDirectory = "Results/wc-new-experiments/old-atom-gens/UMAD/wc"





# This allows this script to take a command line argument for outputDirectory
if len(sys.argv) > 1 and sys.argv[1] != "brief":
    outputDirectory = sys.argv[1]


outputFilePrefix = "log"
outputFileSuffix = ".txt"



errorType = "float"

# Some functions
def median(lst):
    if len(lst) <= 0:
        return False
    sorts = sorted(lst)
    length = len(lst)
    if not length % 2:
        return (sorts[length / 2] + sorts[length / 2 - 1]) / 2.0
    return sorts[length / 2]

def mean(nums):
    if len(nums) <= 0:
        return False
    return sum(nums) / float(len(nums))

def reverse_readline(filename, buf_size=8192):
    """a generator that returns the lines of a file in reverse order
       From: https://stackoverflow.com/questions/2301789/read-a-file-in-reverse-order-using-python"""
    with open(filename) as fh:
        segment = None
        offset = 0
        fh.seek(0, os.SEEK_END)
        total_size = remaining_size = fh.tell()
        while remaining_size > 0:
            offset = min(total_size, offset + buf_size)
            fh.seek(-offset, os.SEEK_END)
            buffer = fh.read(min(remaining_size, buf_size))
            remaining_size -= buf_size
            lines = buffer.split('\n')
            # the first line of the buffer is probably not a complete line so
            # we'll save it and append it to the last line of the next buffer
            # we read
            if segment is not None:
                # if the previous chunk starts right from the beginning of line
                # do not concact the segment to the last line of new chunk
                # instead, yield the segment first 
                if buffer[-1] is not '\n':
                    lines[-1] += segment
                else:
                    yield segment
            segment = lines[0]
            for index in range(len(lines) - 1, 0, -1):
                if len(lines[index]):
                    yield lines[index]
        yield segment


# Main area
i = 0

if outputDirectory[-1] != '/':
    outputDirectory += '/'
dirList = os.listdir(outputDirectory)

print
print "           Directory of results:"
print outputDirectory

bestFitnessesOfRuns = []
testFitnessOfBest = []
testFitnessOfSimplifiedBest = []
errorThreshold = maxint
errorThresholdPerCase = maxint
numCases = maxint

fileName0 = (outputFilePrefix + str(i) + outputFileSuffix)
f0 = open(outputDirectory + fileName0)

for line in f0:
    if line.startswith("error-threshold"):
        try:
            errorThreshold = int(line.split()[-1])
        except ValueError, e:
            errorThreshold = float(line.split()[-1])
        if errorThreshold == 0:
            errorThresholdPerCase = 0

    if errorThresholdPerCase == maxint and line.startswith("Errors:"):
        numCases = len(line.split()) - 1
        errorThresholdPerCase = float(errorThreshold) / numCases

    if errorThresholdPerCase != maxint and numCases != maxint:
        break

while (outputFilePrefix + str(i) + outputFileSuffix) in dirList:
    sys.stdout.write("%4i" % i)
    sys.stdout.flush()
    if i % 25 == 24:
        print

    runs = i + 1 # After this loop ends, runs should be correct
    fileName = (outputFilePrefix + str(i) + outputFileSuffix)
#    f = open(outputDirectory + fileName)

    #final = False
    gen = 0
    best_mean_error = maxint
    done = False

    bestTest = maxint
    simpBestTest = maxint

    if os.path.getsize(outputDirectory + fileName) == 0:
        bestFitnessesOfRuns.append((gen, best_mean_error, done))
        testFitnessOfBest.append(bestTest)
        testFitnessOfSimplifiedBest.append(simpBestTest)
        i += 1
        continue

    for line in reverse_readline(outputDirectory + fileName):

        if line.startswith(";; -*- Report") and gen == 0:
            gen = int(line.split()[-1])

        if gen != 0 and best_mean_error < maxint:
            break

        if line.startswith("SUCCESS"):
            done = "SUCCESS"

        if line.startswith("FAILURE"):
            done = "FAILURE"

        if line.startswith("Mean:"):
            gen_best_error = -1
            if errorType == "float":
                gen_best_error = float(line.split()[-1])
            elif errorType == "int" or errorType == "integer":
                gen_best_error = int(line.split()[-1])
            else:
                raise Exception("errorType of %s is not recognized" % errorType)

            if gen_best_error < best_mean_error:
                best_mean_error = gen_best_error

        if line.startswith("Test total error for best:") and done:
            try:
                bestTest = int(line.split()[-1].strip("Nn"))
            except ValueError, e:
                bestTest = float(line.split()[-1].strip("Nn"))

        if line.startswith("Test total error for best:") and not done:
            try:
                simpBestTest = int(line.split()[-1].strip("Nn"))
            except ValueError, e:
                simpBestTest = float(line.split()[-1].strip("Nn"))


    bestFitnessesOfRuns.append((gen, best_mean_error, done))
    testFitnessOfBest.append(bestTest)
    testFitnessOfSimplifiedBest.append(simpBestTest)
            
    i += 1

print

if verbose:
    print "Error threshold per case:", errorThresholdPerCase
    print "-------------------------------------------------"

    for i, (gen, fitness, done) in enumerate(bestFitnessesOfRuns):
        doneSym = ""
        if fitness <= errorThresholdPerCase:
            doneSym = " <- suc"
            if not done:
                doneSym += "$$$$$$ ERROR $$$$$$" #Should never get here
            if len(testFitnessOfBest) > i and testFitnessOfBest[i] < maxint:
                if isinstance(testFitnessOfBest[i], int):
                    doneSym += " | test = %i" % testFitnessOfBest[i]
                else:
                    doneSym += " | test = %.3f" % testFitnessOfBest[i]
            if len(testFitnessOfSimplifiedBest) > i and testFitnessOfSimplifiedBest[i] < maxint:
                if isinstance(testFitnessOfSimplifiedBest[i], int):
                    doneSym += " | test on simplified = %i" % testFitnessOfSimplifiedBest[i]
                else:
                    doneSym += " | test on simplified = %.4f" % testFitnessOfSimplifiedBest[i] #TMH this line changed
        elif not done:
            doneSym = " -- not done"
        if fitness >= 0.001 or fitness == 0.0:
            print "Run: %3i  | Gen: %5i  | Best Fitness (mean) = %8.4f%s" % (i, gen, fitness, doneSym)
        else:
            print "Run: %3i  | Gen: %5i  | Best Fitness (mean) = %.4e%s" % (i, gen, fitness, doneSym)

totalFitness = 0
inds = 0
perfectSolutions = 0
perfectOnTestSet = 0
simpPerfectOnTestSet = 0
trainSolutionGens = []
testSolutionGens = []

for i, (gen, fitness, done) in enumerate(bestFitnessesOfRuns):
    if done:
        totalFitness += fitness
        inds += 1
        if fitness <= errorThresholdPerCase:
            perfectSolutions += 1
            trainSolutionGens.append(gen)
            if len(testFitnessOfBest) > i:
                if testFitnessOfBest[i] <= errorThresholdPerCase:
                    perfectOnTestSet += 1
                    testSolutionGens.append(gen)
            if len(testFitnessOfSimplifiedBest) > i:
                if testFitnessOfSimplifiedBest[i] <= errorThresholdPerCase:
                    simpPerfectOnTestSet += 1

if verbose and len(trainSolutionGens) > 0:
    print "------------------------------------------------------------"
    print "Training Solution Generations:"
    print "Mean:      ", mean(trainSolutionGens)
    print "Minimum:   ", min(trainSolutionGens)
    print "Median:    ", median(trainSolutionGens)
    print "Maximum:   ", max(trainSolutionGens)

    if len(testSolutionGens) > 0:
        print "--------------------------"
        print "Test Solution Generations:"
        print "Mean:      ", mean(testSolutionGens)
        print "Minimum:   ", min(testSolutionGens)
        print "Median:    ", median(testSolutionGens)
        print "Maximum:   ", max(testSolutionGens)

        # print testSolutionGens

print "------------------------------------------------------------"

print "Number of finished runs:            %4i" % inds
print "Solutions found:                    %4i" % (perfectSolutions)
print "Zero error on test set:             %4i" % perfectOnTestSet
print "Simplified zero error on test set:  %4i" % simpPerfectOnTestSet

print "------------------------------------------------------------"

if inds > 0:
    print "MBF: %.5f" % (totalFitness / float(inds))
