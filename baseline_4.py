
def numfix(inferences):
    'Return the number of positions with fixed colors.'
    if len(inferences) == 0:
        return 0
    count = 0
    for element in inferences:
        if len(element[1]) == 1:
            count += 1
    return count
    
################################################################################

def tied(position):
    'Returns true if position has a color tied to it.'
    'Positions start from 1 to the number of pegs.'
    if len(inferences) == 0:
        return False
    for element in inferences:
        if len(element[1]) == 1 and position in element[1]:
            return True
    return False

def itscolor(i):
    'i is position, from 0 to number of pegs'
    for element in inferences:
        if length(element[1]) == 1 and i in element[1]:
            return element[0]

def length(l):
    return len(l)

def nextpos(i):
    # i is a color
    # returns the first possible possible of color i
    for element in inferences:
        if element[0] == i and len(element[1]) > 1:
            # (and length of element[1] is > 1, but this is assumed)
            return element[1][0]

def secondunfixed(inferences):
    first = False
    for element in inferences:
        if len(element[1]) != 1 and first == False:
            first = True
        elif len(element[1]) != 1 and first == True:
            return element[0]
            
# print(secondunfixed(inferences))

def getnext():
    # beingfixed is the next color to be fixed
    # beingconsidered is the next color to be guessed, to determine its count
    # inferences starts off as an empty list, expands as each color peg is added
    guess = []
    print()
    for x in range(1, pegs + 1):
        # x is positions starting from 1 to number of pegs
        if tied(x):
            # if position x has a color tied to it
            print('tied')
            guess.append(itscolor(x))
        elif x == nextpos(beingfixed):
            # if position x is equal to the next position of the color beingfixed
            print('nextpos')
            guess.append(beingfixed)
        elif length(inferences) == pegs:
            # if all the colors have been guessed, need to be fixed
            print('only fix left')
            guess.append(secondunfixed(inferences))
        else:
            print('beingconsidered')
            guess.append(beingconsidered)
    return guess
    
################################################################################

def addlists(gain, beingconsidered, inferences):
    'Add "gain" number of lists to inferences'
    'Global variables pegs, positions should start from 1'
    positions = []
    for i in range(pegs):
        positions.append(i + 1)
    for x in range(gain):
        inferences.append([beingconsidered, positions])
    return inferences

def fix(beingfixed, inferences):
    position = -1
    first = False
    for element in inferences:
        if element[0] == beingfixed and len(element[1]) > 1 and first == False:
            # fix the first unfixed instance of the color
            element[1] = element[1][:1]
            position = element[1][0]
            first = True
    if position != -1:
        for element in inferences:
            if len(element[1]) > 1 and position in element[1]:
                element[1].remove(position)
    return inferences

#inferences = [[3, [2, 4, 5]], [4, [2, 3, 4, 5]], [7, [1, 2, 3, 4, 5]]]
#print(fix(3, inferences))

def bump(inferences):
    for element in inferences:
        if len(element[1]) > 1:
            return element[0]
            
# inferences = [[3, [2]], [4, [3, 4, 5]], [7, [1, 3, 4, 5]]]
# print(bump(inferences))

def delete(color1, color2, inferences):
    'Delete the first possible position of color1 from all? the sublists for color2'
    position = -1
    for element in inferences:
        if color1 == element[0] and len(element[1]) > 1:
            position = element[1][0]
            break
    if position != -1:
        for element in inferences:
            if color2 == element[0] and position in element[1]:
                element[1].remove(position)
    return inferences
    
# inferences = [[2, [3, 4, 5]], [3, [2, 3, 4, 5]]]
# print(delete(2, 3, inferences))

def fix1(color1, color2, inferences):
    'Fix color1 in the first position of color2.'
    for element in inferences:
        if element[0] == color2 and len(element[1]) > 1:
            position = element[1][0]
    for element in inferences:
        if element[0] == color1 and len(element[1]) > 1:
            element[1] = [position]
        elif len(element[1]) > 1 and position in element[1]:
            element[1].remove(position)
    return inferences
    
# inferences = [[2, [3, 4, 5]], [3, [2, 3, 4, 5]], [4, [2, 3, 4, 5]]]
# print(fix1(2, 3, inferences))

def cleanup(inferences):
    queue = []
    for element in inferences:
        if length(element[1]) == 1:
            queue.append(element[1][0])
    # print(queue)
    while queue != []:
        position = queue.pop()
        for element in inferences:
            # print(element)
            if length(element[1]) > 1 and position in element[1]:
                element[1].remove(position)
                if length(element[1]) == 1:
                    queue.append(element[1][0])
    return inferences

# inferences = [[2, [3]], [4, [3, 4, 5]], [5, [3, 5]]]
# print(cleanup(inferences))

def nextcolor(beingconsidered):
    'Return the next color to be considered. If bigger than number of colors, return -1.'
    'Colors start from 0, unlike the paper.'
    'colors is a global variable here'
    if beingconsidered == -1:
        return -1
    beingconsidered += 1
    if beingconsidered > colors:
        return -1
        # start the counts from 1 instead of 0?
    return beingconsidered
    

def update(beingfixed, beingconsidered, inferences, guess, bulls, cows):

    if cows == 0:
        inferences = fix(beingfixed, inferences)
        beingfixed = bump(inferences)
    elif cows == 1:
        if beingfixed != 0:
            delete(beingfixed, beingconsidered, inferences)
        delete(beingfixed, beingfixed, inferences)
    elif cows == 2:
        inferences = fix1(beingfixed, beingconsidered, inferences)
    else:
        pass
        # ? cows will never be greater than 2?
    
    gain = bulls - numfix(inferences)
    # print(beingconsidered, pegs)
    if beingconsidered == colors and len(inferences) < pegs:   # not completely sure why this is needed
        gain += 1
    print('gain:', gain)
    # Addlists to inferences here? Why isn't this in the pseudocode?
    inferences = addlists(gain, beingconsidered, inferences)
    # print(inferences)
    
    inferences = cleanup(inferences)
    beingconsidered = nextcolor(beingconsidered)
    beingfixed = bump(inferences)
    
    return beingfixed, beingconsidered, inferences

################################################################################

def score_guess(code, guess):
    # "try"  in the paper
    guess2 = []
    code2 = []
    bulls = 0
    cows = 0
    for n, element in enumerate(guess):
        if guess[n] == code[n]:
            bulls += 1
        else:
            guess2.append(guess[n])
            code2.append(code[n])
    for element in guess2:
        if element in code2:
            code2.remove(element)
            cows += 1
    # print(code, guess, bulls, cows)
    return bulls, cows


colors = 8
pegs = 5

code = [2, 3, 2, 4, 6]
code = [8, 7, 6, 5, 4]

beingfixed = 0
beingconsidered = 1

guess = []
for x in range(pegs):
    guess.append(1)

inferences = []

print('\n\n\n\n\n\n\n\n\n\n')

for x in range(12):
# while score_guess(code, guess)[0] < pegs:
    print(guess)
    bulls, cows = score_guess(code, guess)
    print('bulls, cows', bulls, cows)
    beingfixed, beingconsidered, inferences = update(beingfixed, beingconsidered, inferences, guess, bulls, cows)
    print('beingfixed:', beingfixed)
    print('beingconsidered:', beingconsidered)
    for element in inferences:
        print(element)
    print('update done')
    
    guess = getnext()
    print('\n')
    print('next guess:', guess)
    
    print('\n\n')

