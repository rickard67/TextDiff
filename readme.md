# Text compare component for Delphi and Free Pascal

Source code for the freeware component TDiff is written in Delphi. This 
component dramatically simplify programming tasks that require calculations
of 'shortest path' or 'longest common sequence' as typically required in file 
compare utilities.  

PDF documents fully describing the principle algorithms used in both the new and 
the older TDiff component ... The original TDiff component was based on: 
"An O(ND) Difference Algorithm and its Variations" by E Myers - 
Algorithmica Vol. 1 No. 2, 1986, pp. 251-266
The current TDiff component is based on: "An O(NP) Sequence Comparison Algorithm"
by Sun Wu, Udi Manber & Gene Myers

### Getting Started

Download the files and include the Pascal files in your project.

### Demo applications

Two very simple demo applications (with full source code) are include to
demonstrate how the 'TDiff' component can be used in Delphi and Lazarus programs.

### Authors
Author        : Angus Johnson - angusj-AT-myrealbox-DOT-com\
Copyright     : © 2001-2008 Angus Johnson\
Updated by    : Rickard Johansson ([RJ TextEd](https://www.rj-texted.se))

### Version history
- December 2001 - Original release (used Myer O(ND) Difference Algorithm) 
- 22 April 2008    - Complete rewrite to greatly improve the code and provide a much simpler view of differences through a new 'Compares' property.
- 21 May 2008      - Another complete code rewrite to use Sun Wu et al. O(NP) Sequence Comparison Algorithm which more than halves times of typical comparisons.
- 24 May 2008      - Reimplemented "divide-and-conquer" technique (which was omitted in 21 May release) so memory use is again minimal.
- 25 May 2008      - Removed recursion to avoid the possibility of running out of stack memory during massive comparisons.
- 2 June 2008      - Minor bugfixes.
- 11 November 2018 - Added TList<Cardinal> to store hash values.\
                     Made some minor code formatting and code changes\
                     Fixed Unicode string issues
- 19 May 2020        Added Lazarus support
- 23 May 2020        Replaced almost all code in demo 1. It should be much easier to understand now.\
                     Fixed a few issues in demo 2.

