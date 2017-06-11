module Messages
(
    invalidRaiseAmount,
    raiseNotInteger,
    badCheckInput,
    badCallInput,
    inputRaise,
    inputCheck,
    inputCall
)
where

invalidRaiseAmount :: String
invalidRaiseAmount = "Bad input, a raise must be as large as the previous \
                     \raise, or larger."

raiseNotInteger :: String
raiseNotInteger = "Bad input, please enter an integer to raise to."

badCheckInput :: String
badCheckInput = "Bad input, please enter check, or raise."

badCallInput :: String
badCallInput = "Bad input, please enter fold, call, or raise."

inputRaise :: String
inputRaise = "What do you want to raise to? :"

inputCheck :: String
inputCheck = "Check, or raise?: "

inputCall :: String
inputCall = "Fold, call, or raise?: "
