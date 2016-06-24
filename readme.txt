======================================================================= 
Steven Wopschall, June 24, 2016
======================================================================= 
Introduction
======================================================================= 
The "technical_exercise_srw" directory contains the code, written by 
Steven Wopschall, for the Plethora technical exercise.  This code was 
submitted on June 24, 2016.

This document explains how the code works, and the libraries/programs,
etc., necessary to run the code.

======================================================================= 
Code Overview
======================================================================= 
Source Files:

The source files are in the "te_json_src" directory.  The main program
is "te_srw.f" with an executable "te_srw.x".  The guts of the code are 
contained in the "cut_cost_m.f" module, which contain the derived 
type definitions and all of the type bound procedures necessary for 
this technical exercise.

Makefile:

The "makefile" is used to compile the code.  This is done simply by
typing "make".  To remove the .o and .mod files created by "make", 
one may type "make clean".

Compiler Notes:

This code was written, compiled, tested/run using GCC Version 5.3.0

External Library Notes:

This code uses the JSON-FORTRAN library, available here:
https://github.com/jacobwilliams/json-fortran.  This is a modern 
fortran json file parser and is used to deserialize the json input 
file.  The library is contained in the json-fortran-master directory 
and is included in this submission.  Specifics about precompiling the 
library are included in a subsequent section.
 
Tests/File Input:

The three tests provided in the Plethora technical exercise are 
included in the directory, "tests".  These were the only files with
which the code was tested. To run the code with a specific file as 
input, one must copy that file and rename it, "test_input.json". 

Output: 

The cost of cutting a particular part is output to the terminal 
window.  As a note, the code produces the exact output for the three 
test cases based on the "solutions" provided in the Plethora 
technical exercise.

======================================================================= 
JSON-FORTRAN Library
======================================================================= 

The JSON-FORTRAN library is contained in the directory,
json-fortran-master.  It is precompiled using a shell script entitled, 
"build.sh".  To run this script, one must download a python code, 
FoBiS, onto their machine as there are calls to FoBiS.py in build.sh.
FoBiS can be found here: https://github.com/szaghi/FoBiS.  FoBiS.py 
does not need to be installed in any particular directory for build.sh 
to find it.  More information about JSON-FORTRAN is found on the 
aforementioned github link, including build information.  The library 
must be in the "json-fortran-master" subdirectory in the 
"technical_exercise_srw" directory in order for the code to run. 
Additionally, the subdirectory "lib" in "json-fortran-master" should 
contain all of the .mod files and the libary's .a file. 

======================================================================= 
Notes on Code Improvements
======================================================================= 
General:

The code, in particular the subroutines, are commented in a way that 
indicates advantageous and disadvantageous characteristics.  Some of 
these will be highlighted here.

Derived Types and Data Structures:

There is duplicate data storage in that the lowest order derived type
that I use to store shape information is an "Edge" derived type.  
Each edge object contains data specific to the edge vertices.  Clearly 
two adjacent edges share a common vertex.  A more streamlined approach 
would be to have the lowest order derived type be a vertex where an 
edge would "index" into vertex data.  

Type Bound Procedures:

All of the subroutines in CUT_COST_M.f are type bound procedures, most 
of which are on the CUT_SHAPE derived type/object.  There is no 
additional input to any of the procedures and each procedure populates 
component variables on the derived type to which that procedure is 
bound.  In all cases this is explained in the code.  Some of the 
routines could be abstracted, such as the convex hull and minimum 
oriented bounding box routine to general subroutines that can be used 
in other code as well.  This would make the code I have written more
readily useful for future projects.

Convex Hull Algorithm:

I could not successfully find a convex hull subroutine in a geometric 
library or otherwise that could be easily incorporated into my code 
in the timeframe alotted; as a result, I wrote my own.  Having done 
so requires some brief comments.  

Any arbitrary shape may be convex or nonconvex, and my include any 
number of extruded circular arcs.  The convex hull algorithm was 
designed and written to handle nonconvex shapes, but has NOT 
been tested on a nonconvex example.  To ensure the robustness of 
the algorithm, this would need to happen.  Additionally, ultimately 
a point set of vertices is the easiest and perhaps most natural 
data set to work with when determining a convex hull and a minimum 
oriented bounding box, but the extruded arc is not defined by a 
point set.  As a result, for a shape with an extruded circular arc, 
I introduce vertices to form a "box" around the arc.  That is, the 
two arc edge vertices connect to an additional pair of vertices 
that form a three sided "box" that bounds the arc.  These new vertices 
are included in the point set of original vertices in order to 
determine the convex hull.  This is really an artificial point set 
used to determine the convex hull, but one that was determined to 
be accurate and sufficient enough, and one that yields correct 
results for the extruded arc example.

For the example problem with an extruded circular arc edge this 
approach is fine, because the outer most dimension of the minimum 
oriented bounding box is at the "top" of the extruded arc.  In a 
more general and complex n-gon where then extruded arc edges have 
been introduced, the arc "box" may result in a convex hull that 
produces a larger "minimum" oriented bounding box than the shape 
actually requires.  Depending on the complexity of the shape and 
the dimensions of the arc, however, this "inaccuracy" may be 
within some acceptable price tolerance.  Future work would consider 
optimizing the current algorithm or implementing a provably 
optimized convex hull algorithm.

Minimum Oriented Bounding Box Algorithm:

This alorgorithm is O(n^2) and may be optimized in the future.  The 
algorithm takes the convex hull point set and assumes a bounding 
box that has an edge coincident with a given edge of the convex 
hull.  From this orientation, maximum height and width dimensions 
are calculated based on the point set and a bounding box area 
is calculated.  This is done for every edge in the convex hull, 
and the one that produces the smallest rectangle is the area 
used in the material cost calculations.  Currently, the shape's 
edge with which the minimum oriented bounding box is aligned 
is not stored; the area is simply calculated, stored, and used. 
This can easily be modified.  This algorithm was not tested 
on arbitrary n-gons, but produces the correct minimum bounding 
box areas for the three example cases.  The simplicity of the 
algorithm in part contributes to the time complexity, but results 
in a more robust implementation given that all geometric pairings 
are considered.
======================================================================= 

