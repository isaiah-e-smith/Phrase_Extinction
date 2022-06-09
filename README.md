# Phrase_Extinction

A disclaimer: I am a palaeontologist and and evolutionary biologist, not a linguist.

This is a hobby project to satisfy my dilettante's interest in the evolution of language. This project is (more-or-less) a greatly-simplified adaptation of palaeontological analyses I've run on marine organisms, applied to the English language.


A bit of background... 

Much like biological species, languages evolve, migrate, and go extinct. On a smaller scale, individual words can "go extinct" in the sense that they 1.) fall almost entirely out of usage or 2.) change in definition such that the new meaning becomes widely accepted. For example, the English word "nice" meant "foolish" during Shakespeare's time, whereas today this word is widely considered to have a completely different meaning (source: A History of the English Language by Albert C. Baugh and Thomas Cable, 6th Edition, page 3). Although a word can be"revived" after decades or centuries of little-to-no use, perhaps it goes "functionally extinct" when its usage frequency drops below a certain threshold. What this "functional extinction" means exactly, I am not entirely sure. Perhaps it means that the word is almost entirely removed from everyday speech, or perhaps it opens up the possibility of the word taking on a new meaning. Unlike biological species, however, an extinct word can easily be brought back to life.


This script will investigate the relationship between instantaneous usage frequency and trajectory of usage frequency as it relates to functional "word extinction" (within evolutionary biology, this trend isn't always as intuitive as one might think). A "word extinction" is assumed when usage frequency drops below an arbitrary minimum threshold. It is also worth noting that, for now, this analysis does not group together different grammatical forms of the same word (you RUN, you are RUNNING, you RAN; or, one DOG, two DOGS), when in reality these should likely be treated as a single entity. Nonetheless, if we assume a relatively constant usage ratio of the different grammatical forms of a word, a single form could serve as a representative metric for overall word usage through time.

This script was written to account for missing data. Although most of the ngram results from Google will not be missing data, I tried to write the script such that it could be used with other data sources, which, for whatever reason, may have gaps in the data.
