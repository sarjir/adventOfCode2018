import Browser
import Set exposing (Set)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Frequency exposing (frequencyInput)

main =
  view "Hello"


addFrequency : Int -> Int -> Int
addFrequency currentFrequency frequency =
  currentFrequency + frequency

updateFrequencySet : Set Int -> Int -> Set Int
updateFrequencySet frequencyCombos newFreq =
  Set.insert newFreq frequencyCombos

checkCombos : Set Int -> Int -> Bool
checkCombos frequencyCombos frequency =
  Set.member frequency frequencyCombos

calculateFirstDuplicatedFrequency : List Int -> List Int -> Set Int -> Int -> Int
calculateFirstDuplicatedFrequency originalFrequencies frequenciesModifiers frequencyCombos currentFrequency =
  let 
    createNewFreq = addFrequency currentFrequency
    updateCombos = updateFrequencySet frequencyCombos
    checkFrequencyCombos = checkCombos frequencyCombos
  in 
    case frequenciesModifiers of
      [] ->  0
      [x] ->
        calculateFirstDuplicatedFrequency originalFrequencies originalFrequencies (updateCombos (createNewFreq x)) (createNewFreq x)
      (x::xs) ->
        if checkFrequencyCombos (createNewFreq x) then
          createNewFreq x

        else
          calculateFirstDuplicatedFrequency originalFrequencies xs (updateCombos (createNewFreq x)) (createNewFreq x)

calculateFrequency : List Int -> Int
calculateFrequency frequencies =
  List.sum frequencies

createText : Int -> Html msg
createText finalNumber = 
  div [] [ text (String.fromInt finalNumber) ]

view : String -> Html msg
view model =
  div []
    [ text "First puzzle"
    , createText (calculateFrequency frequencyInput)
    , text "Second puzzle"
    , createText (calculateFirstDuplicatedFrequency frequencyInput frequencyInput Set.empty 0)
    ]