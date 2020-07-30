This zip file contains ...

1. Source code for the freeware component - TDiff - written in 
   Borland's Delphi programming language. This component dramatically 
   simplify programming tasks that require calculations of 'shortest path' or 
   'longest common sequence' as typically required in file compare utilities. 

2. Two very simple demo applications (with full source code) that 
   demonstrate how the 'TDiff' component can be used in Delphi programs. 

3. A fully featured text compare utility 'TextDiff' (without source code) 
   which showcases the speed and accuracy of the TDiff component. The
   utility (and accompanying help file) requires no installation apart from
   extracting from this zip file.

4. PDF documents fully describing the principle algorithms used in both the
   new and the older TDiff component ...
	 The original TDiff component was based on:
     "An O(ND) Difference Algorithm and its Variations"
	   by E Myers - Algorithmica Vol. 1 No. 2, 1986, pp. 251-266
	 The current TDiff component is based on:
     "An O(NP) Sequence Comparison Algorithm"
     by Sun Wu, Udi Manber & Gene Myers
   
------------------------------------------------------------------------------

## TDiff Component ##
Version       : 5.01
Last updated  : 19 May 2020
Compilers     : Delphi 10.x
Author        : Angus Johnson - angusj-AT-myrealbox-DOT-com
Copyright     : © 2001-2008 Angus Johnson
Updated by    : Rickard Johansson (RJ TextEd)

History:                                                                     
13 December 2001 - Original release (used Myer's O(ND) Difference Algorithm) 
22 April 2008    - Complete rewrite to greatly improve the code and          
                   provide a much simpler view of differences through a new  
                   'Compares' property.                                      
21 May 2008      - Another complete code rewrite to use Sun Wu et al.'s      
                   O(NP) Sequence Comparison Algorithm which more than       
                   halves times of typical comparisons.                      
24 May 2008      - Reimplemented "divide-and-conquer" technique (which was   
                   omitted in 21 May release) so memory use is again minimal.
25 May 2008      - Removed recursion to avoid the possibility of running out
                   of stack memory during massive comparisons.
2 June 2008      - Minor bugfixes.
11 November 2018 - Added TList<Cardinal> to store hash values
                   Made some minor code formatting and code changes
                   Fixed Unicode string issues
19 May 2020        Added Lazarus support
23 May 2020        Replaced almost all code in demo 1. It should be much easier to understand now.
                   Fixed a few issues in demo 2.
------------------------------------------------------------------------------

Why did I changed difference algorithms? 
The short answer is that the new one is faster.

The original "D" algorithm has been replaced by the "P" algorithm 
(see accompanying PDF documents for their full descriptions). Both
algorithms accurately derive a "shortest edit script" or "edit path" (and 
conversely a "longest common subsquence"), but the "P" algorithm is 
significantly faster. However, on their own, both algorithms require 
exponential amounts of memory to map out an optimal "edit path". To 
circumvent this problem of consuming very large amounts of memory, a 
'divide-and-conquer' technique can be used. By applying the difference 
algorithms simultaneously from both ends of the difference arrays, a 
midpoint of an optimal "edit path" can be derived. Then by repeatedly finding
"edit path" midpoints of subsections of a whole "edit path", an entire 
"edit path" can be defined. However, only the original "D" algorithm is 
optimally suited to this 'divide-and-conquer' approach. Applying the new "P" 
algorithm simultaneously from both ends of difference arrays does not alway 
reveal a midpoint that's exactly on an optimal "edit path". This means that 
very occasionally matches within surrounding differences may get missed. 
Fortunately in practice these missed 'matches' are of little or no 
significance as they're always embedded within larger blocks of changes. 
As an example, consider the following 2 phrases - "a child swims in the creek" 
and "a frog hopped in the creek". The two phrases share several matching 
words - "a ... in the creek" - and also a matching space between the two 
changed words. In a more complex example the faster "P" algorithm using 
the 'divide-and-conquer' memory sparing technique might miss that matching 
space and report that the dissimilarities encompass both changed words and 
that space. Most importantly however, while matches may very occasionally be
missed, there's no risk of differences being missed when using the "P" 
algorithm with 'divide-and-conquer'. Anyhow, I believe the speed benefits of 
the new "P" algorithm generally outweigh an occasional missed match. 
Nevertheless, in case these occasional missed matches are considered 
important by some, I've included the older TDiff implementation 
(using the "D" algorithm) in this package so it can still be used 
instead of the newer implementation.

------------------------------------------------------------------------------

## TextDiff utility ##
Version       : 4.5
Last updated  : 2 June 2008
Copyright     : © 2001-2008 Angus Johnson
License       : Freeware
Installation  : Just extract the TextDiff.exe and TextDiff.hlp files and run.

While now a fairly comprehensive file & folder compare/merge utility, it was 
originally written to showcase the speed and accuracy of the TDiff component.

------------------------------------------------------------------------------

Licence to use, terms and conditions:
	The source code in this package is released as freeware
	provided you agree to the following terms & conditions:
	1. copyright notices, terms and conditions are left unchanged.
	2. modifications to the code by other authors must be
	clearly documented and accompanied by the modifier's name.
	3. the code may be freely compiled into binary format 
	and while acknowledgement is appreciated, it isn't required.
	
------------------------------------------------------------------------------
