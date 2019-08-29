##Using an Updated Genetic Algorithm to Solve the “Mastermind” Puzzle
 
#Genetic Algorithm Based on Eliteness Pseudo Code
 
Set i = 1
Play fixed initial guess g1:
Get response X1 and Y1;
while Xi ≠ Pegs do
    i = i + 1;
    Set (Êi) = {} and h = 1;
    Initialize population:
    while (h ≤ maxgen) do
        Calculate and sort population by fitness;
       Push elite percent of past generation into new generation;
           while (|Êi | ≤ maxsize)
    Generate child using crossover, mutation, inversion, and permutation of top 50
                   percent of past generation;
             If child is not a duplicate, push child to new generation;
        end while
    h = h + 1;
    end while
    Play guess gi first element of (Êi);
    Get response Xi (bulls) Yi (cows)
end while

