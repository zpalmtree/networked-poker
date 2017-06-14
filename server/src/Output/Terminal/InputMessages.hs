module Output.Terminal.InputMessages
(
    invalidRaiseAmount,
    raiseNotInteger,
    badCallInput,
    badFoldAllInInput,
    badCheckAllInInput,
    badFoldCallAllInInput,
    badCheckRaiseAllInInput,
    badFoldCallRaiseAllInInput,
    inputRaise,
    inputFoldAllIn,
    inputCheckAllIn,
    inputFoldCallAllIn,
    inputCheckRaiseAllIn,
    inputFoldCallRaiseAllIn,
)
where

invalidRaiseAmount :: String
invalidRaiseAmount = "Bad input, a raise must be as large as the previous \
                     \raise, or larger, and you must have enough chips to \
                     \make the raise."

raiseNotInteger :: String
raiseNotInteger = "Bad input, please enter an integer to raise to."

badCallInput :: String
badCallInput = "Bad input, please enter fold, call, or raise."

badFoldAllInInput :: String
badFoldAllInInput = "Bad input, please enter fold, or all in."

badCheckAllInInput :: String
badCheckAllInInput = "Bad input, please enter check, or all in."

badFoldCallAllInInput :: String
badFoldCallAllInInput = "Bad input, please enter fold, call, or all in."

badCheckRaiseAllInInput :: String
badCheckRaiseAllInInput = "Bad input, please enter check, raise, or all in."

badFoldCallRaiseAllInInput :: String
badFoldCallRaiseAllInInput = "Bad input, please enter fold, call, raise, or \
                             \all in."

inputRaise :: String
inputRaise = "Player number %d, %s, What do you want to raise to?: "

inputFoldAllIn :: String
inputFoldAllIn = "Player number %d, %s, Fold, or all in?: "

inputCheckAllIn :: String
inputCheckAllIn = "Player number %d, %s, Check, or all in?: "

inputFoldCallAllIn :: String
inputFoldCallAllIn = "Player number %d, %s, Fold, call, or all in?: "

inputCheckRaiseAllIn :: String
inputCheckRaiseAllIn = "Player number %d, %s, Check, raise, or all in?: "

inputFoldCallRaiseAllIn :: String
inputFoldCallRaiseAllIn = "Player number %d, %s, Fold, call, raise, or all in?: "
