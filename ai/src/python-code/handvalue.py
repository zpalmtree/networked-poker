from deuces import Deck, Card, Evaluator

# royal flush
bestHandValue = 1
# unsuited 7-5-4-3-2
worstHandValue = 7462

def generateData():
    iterations = 1
    evaluator = Evaluator()

    for _ in range(0, iterations):
        deck = Deck()

        # can draw 5 cards from a 52 card deck 10 times, should be faster than
        # just making a new deck
        for _ in range(0, 10):
            board = deck.draw(5)
            handVal = evaluator.evaluate(board, [])

            cards = map(Card.int_to_str, board)
            vector = list(handToVector(cards))
            print(vector)
            print(normaliseHandValue(handVal))

def normaliseHandValue(handValue):
    return (handValue - 1) / (worstHandValue - 1)

def splitCard(card):
    return "{}-{}".format(valueToInt(card[0]), suitToInt(card[1]))

def valueToInt(value):
    namedCards = { 'A' : 12, 'K' : 11, 'Q' : 10, 'J' : 9, 'T' : 8, }

    if value in namedCards:
        return namedCards[value]
    else:
        return int(value) - 2

def suitToInt(suit):
    return {
        'h' : 0,
        's' : 1,
        'd' : 2,
        'c' : 3
    }[suit]

def makeInputVector(value, maxValue):
    initial = [0] * (maxValue + 1)
    initial[value] = 1
    return initial

def cardToVector(card):
    maxValueValue = 12
    maxSuitValue = 3

    value = valueToInt(card[0])
    suit = suitToInt(card[1])

    valueV = makeInputVector(value, maxValueValue)
    suitV = makeInputVector(suit, maxSuitValue)

    return [valueV, suitV]

def handToVector(hand):
    return concat(map(cardToVector, hand))

def concat(xs):
    return [item for sublist in xs for item in sublist]
