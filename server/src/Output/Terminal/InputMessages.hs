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
    lessThanMinimumRaise,
    notEnoughChips
)
where

import Text.Printf (printf)

invalidRaiseAmount :: String
invalidRaiseAmount = "Bad input, a raise must be as large as the previous " ++
                     "raise, or larger, and you must have enough chips to " ++
                     "make the raise."

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
inputRaise = "Player %d, %s, what do you want to raise to? Your current bet " ++
             "is %d, and you have %d chips left in your stack: "

inputFoldAllIn :: String
inputFoldAllIn = "Player %d, %s, would you like to fold, or go all in? Your " ++
                 "current bet is %d, and you have %d chips left in your " ++
                 "stack: "


inputCheckAllIn :: String
inputCheckAllIn = "Player %d, %s, would you like to check, or go all in? " ++
                  "Your current bet is %d, and you have %d chips left in " ++
                  "your stack: "

-- lol misusing printf because putting spaces in my strings looks ugly
inputFoldCallAllIn :: Int -> String
inputFoldCallAllIn call = printf "%s %d, %s" start call end
    where start = "Player %d, %s, would you like to fold, call"
          end = "or go all in? Your current bet is %d, and you have %d " ++
                "chips left in your stack: "

inputCheckRaiseAllIn :: String
inputCheckRaiseAllIn = "Player %d, %s, would you like to check, raise, or " ++
                       "go all in? Your current bet is %d, and you have %d " ++
                       "chips left in your stack: "

inputFoldCallRaiseAllIn :: Int -> String
inputFoldCallRaiseAllIn call = printf "%s %d, %s" start call end
    where start = "Player %d, %s, would you like to fold, call"
          end = "raise, or go all in? Your current bet is %d, and you have " ++
                "%d chips left in your stack: "

lessThanMinimumRaise :: String
lessThanMinimumRaise = "Bad input, you must raise to at least %d chips."

notEnoughChips :: String
notEnoughChips = "Bad input, you don't have enough chips to raise that " ++
                 "much. You can raise to a maximum of %d chips."
