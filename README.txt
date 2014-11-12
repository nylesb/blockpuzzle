This is the documentation for the blockpuzzle problem solved by greedy, uniform cost, and astar search.  The latest version can be found on my GitHub: github.com/nylesb/blockpuzzle

=== Description of files ===

README.txt: This file.
main.lisp: Contains the actual blockpuzzle program itself.
mp1.lisp: Contains a driver and testcases for the blockpuzzle program.  It will create a dribble file called "mp1.out" to see the results.
mp1.out_test1: A sample output that I created using mp1.lisp.  It was renamed to have _test1 at the end so that if mp1.lisp is run again it won't change the file.
mp1.in_test1: A sample input file which is properly formatted.
.gitignore: My program is version controlled with git, so the ignore file is included.

=== Notes about the code ===

First off, I apologize if the code is somewhat difficult, structurally, to follow.  I do my best to document but given that I don't have a ton of experience with Lisp, I'm not confident in my particular usage of control structures as being "the best way" to implement something, even if they do get the job done.  There is a lot of room for refactoring and optimization.

There are no deviations from the project guidelines, as far as I can tell.  My blockpuzzle runs each of the search methods on my problem, and then outputs a list containing the number of states examined, then the cost to the solution node it found, then the path to the solution node it found.

When printing the path I start with "S_zero" to represent the start state, and then after that I print how many spaces (positive being right, negative being left) I had to move the blank to get to the current state.  For example,
(((O B W) . "S_zero") ((W B O) . 2))
means that I started at state (O B W), the moved the blank space to the right 2 to get to the state (W B O).  I chose this printing to reprsent the internal structure in the code of how I represented movement.

It is also important to note that my implementation of greedy search chooses randomly between equally promising nodes when facing such a decision.  This means that it won't get stuck in an infinite loop, but it also means successive runs on the same original start state might yield slightly different results.

As another note, my UCS and ASTAR search implementations follow the same basic template.  This makes sense because they both look at an open and closed list, but the priority things are given in the lists differ.  I allow my general search template to be called with a key to UCS or ASTAR and then sort my priority queues based just on cost for UCS and then with cost + heuristic for ASTAR.  (Note that UCS and ASTAR still exist as functions in my code, but they're just aliases for calling the search template with an optional parameter.)

It's also worth noting that my problem could solve puzzles where hopping any number of spaces is allowed.  My movement function just takes into consideration how many places the blank needs to move to get to the new state.

=== My heuristic ===

There are many ways to implement a heuristic.  I choose to look at something which I call "inversions."  Whenever we pick two tiles in the current state of our puzzle, if one is white and one is black and the black one is to the left of the white one then we have an inversion.  Simply, this means these two tiles are out of order and need to be fixed before we find a solution.  My heuristic looks at each distinct pair of tiles in my puzzle and counts how many inversions there are.

This heuristic is admissable.  This is true because the heuristic counts the number of inversions and each inversion requires at least cost 2 to fix because one tile must hop over the other.  We can fix two inversions by hopping over two tiles, but then this cost is 3, still greater than what the heuristic predicts.  Since the heuristic is admissable, we are allowed to use it to implement A* search.

A limitation of this heuristic however is that it does not account for the location of the blank.  Yet, there are many states which cannot fix any inversions (and thereby reduce the heuristic) in a single move.  For example, something like (O B B B B W).  Clearly this looks worse than (B B O B B W), but in terms of the heuristic these are both equally as good.   Determining goodness of these types of states was not implemented because it was unclear to me how to figure out how much "better" one was over the other.

As a result, this is a reason why greedy search without randomization would get into an infinite loop.

=== What I learned ===

The biggest thing I learned about these search strategies was how to actually store and represent data as one progresses along the search space.  In particular, I understood the concept of the open and closed lists, but I was unsure how the methods would reconstruct the path to goal node once it actually found the goal node.  This was an issue that I had to solve while working on the project.  For my particular implementation at least, I decided it was bast to make a node data structure to hold onto this temporary information as I worked along the list.  I fear that this makes my program inefficient, but I could not think of any other ways to do this.

Another thing I noticed was just how much better greedy search can be in terms of speed and closeness of optimality it can be.  For example, in one of my puzzles Greedy found a solution after examining 9 moves with cost 19.  Comparatively UCS looked at 602 states to find the optimal solution of cost 18.  It's one thing to read about it being fast, but another to see it actually working on a particular problem.  (Although, I do believe this particular problem with very uniform costs lends itself to a faster greedy search.)

I also didn't realize just how close ASTAR any UCS were until implementing them, even though I knew both of what they did.  I actually just used the same search template with a different priority associated to my open list.

The problem of heuristics was a very real issue for my program as well.  At first I brainstormed a few ways I could go about making my heuristic.  While I was able to come up with my inversion method rather quickly, I can see how there would be a lot of different ways to go about it.  For example, my first idea was actually to just remove the blank and then count how many W and B tiles were mismatched to the solution.  However, I opted not to take this route because I didn't feel like it captured so much the spirit of the problem which was white and black tiles being out of order relative to each other.  Plus, it seemed like h might go up at some point in trying to solve the puzzle, which seemed bad to me.  So, there are a lot of subtleties in heuristics.

Speed and optimization is something that highly interests me and which I am always concerned about when creating an algorithm or a program.  While I didn't focus a ton on optimization for this program, I did try to makie things run better when I could.  Seeing everything come together between huge open lists, time spent computing a heuristic, and the general processing of information and storage of data it was easy to see how problems get out of hand quickly.  I knew problems grew quickly, but I was not expecting a problem of just size 9 to take about 5 seconds.  I thought I could get to at least 50 before size would be such a problem.