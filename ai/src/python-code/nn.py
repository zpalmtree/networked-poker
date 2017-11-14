import tensorflow as tf
import numpy as np

from deuces import Deck, Card, Evaluator

def manualInput():
    inputVector = []

    deck = Deck()
    board = deck.draw(5)
    print("Picked cards: ")
    Card.print_pretty_cards(board)

    evaluator = Evaluator()
    print("Actual hand value: ")
    print(normaliseHandValue(evaluator.evaluate(board, [])))

    cards = map(Card.int_to_str, board)
    inputVector.append(list(handToVector(cards)))

    inputArr = np.array(inputVector)

    return inputArr

def generateData(iterations):
    inputVector = []
    outputVector = []

    evaluator = Evaluator()

    for _ in range(0, iterations//10):
        deck = Deck()

        # can draw 5 cards from a 52 card deck 10 times, should be faster than
        # just making a new deck
        for _ in range(0, 10):
            board = deck.draw(5)
            handVal = evaluator.evaluate(board, [])

            cards = map(Card.int_to_str, board)
            inputVector.append(list(handToVector(cards)))
            outputVector.append([normaliseHandValue(handVal)])

    inputArr = np.array(inputVector)
    outputArr = np.array(outputVector)

    return inputArr, outputArr
    #return inputVector, outputVector

def concat(xs):
    return [item for sublist in xs for item in sublist]

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

    return valueV + suitV

def normaliseHandValue(handValue):
    worstHandValue = 7462
    return (handValue - 1) / (worstHandValue - 1)

def handToVector(hand):
    return concat(map(cardToVector, hand))

# Array of any dimension by 85
# Card input, 17 per card = 13 values + 4 suit
x = tf.placeholder(tf.float32, [None, 85])

# Input -> 85 to Output -> 1 - the rank
W = tf.Variable(tf.zeros([85, 1]))

# not sure what this is
b = tf.Variable(tf.zeros([1]))

y = tf.nn.softmax(tf.matmul(x, W) + b)

y_ = tf.placeholder(tf.float32, [None, 1])

cross_entropy = tf.reduce_mean(-tf.reduce_sum(y_ * tf.log(y), reduction_indices=[1]))

train_step = tf.train.GradientDescentOptimizer(0.01).minimize(cross_entropy)

sess = tf.InteractiveSession()

tf.global_variables_initializer().run()

result = None

for _ in range(1000):
    batch_xs, batch_ys = generateData(100)
    result = sess.run(train_step, feed_dict={x: batch_xs, y_:batch_ys})

predictor = tf.argmax(y, 1)

inputHand = manualInput()

print(sess.run(predictor, feed_dict= { x: inputHand }))
