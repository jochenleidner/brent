# brent
A variation on Brent's (1993) verb subcategorization learner from text,
developed by Jochen Leidner in 1996.

# Description

Brent (1993)'s article proposed extracting subcategorization frames
from raw text (in his case, the Brown Corpus) with a fascinatingly
simple/elegant approach that does not even rely on any POS tagger or
parser. On Christmas Day 1996, I implemented a variant of his method,
but using a deterministic grammar to find the textual patterns that
is based on the Yacc/Bison family of parser generators from compiler
construction.
The tool was implemented on an Atari ST 520+, which was already a
pretty outdated machine (https://en.wikipedia.org/wiki/Atari_ST) at
the time (mine was 10 years old), but I didn't have another box
around and I wanted to implement the paper, which I had read on
the train home, and luckily I had a copy of BYACC, a port of Berkeley
Yacc for TOS, the machine's operating system.

# brent(1)

This directory contains a tool to extract valency lists
in feature structure format from tokenized English text files:

		...
		I
		do
		not
		believe
		that
		I
		could
		have
		fixed
		...
	
		 |
		 |
		 V

		[ Lemma: "doubt", Valencies: <<Clause>> ];
		[ Lemma: "display", Valencies: <<NounPhrase_Obj>> ];

It is an implementation of the rules-base method described in
Brent (1993). 

# Refernces

Brent, Michael R.: From Grammar to Lexicon (1993):
"Unsupervised Learning of Lexical Syntax." 
Computational Linguistics 19(2), 244-262.

# Known Bugs and Limitations

(Statistic filtering not included in this distribution.)

