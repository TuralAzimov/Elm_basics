module TuralAzimov_HelloElm exposing (..)
import Char exposing (toCode) 
import Char exposing (isLower) 
import Char exposing (isUpper) 
import Char exposing (fromCode) 
import Html exposing (Html, text)
import String  exposing (filter) 
import Char exposing (isAlpha)
import List exposing (map)
import Html exposing (strong)
import List exposing (head)
import List exposing (tail)
import Maybe exposing (andThen)
 
encode: Int -> Char -> Char 
encode number letter = if ((isUpper letter) == True) then
 if  ( 64 <(toCode letter) + number && (toCode letter) + number < 91)   then fromCode((toCode letter) + number) 
 else if ( (toCode letter) + number < 65) then  fromCode (90+(toCode letter)-number-65+1)  
   else   fromCode((toCode letter)-90+number+65-1)
 else if ((isLower letter) == True) then 
 if  ( 96 < (toCode letter) + number && (toCode letter) + number < 123)   then fromCode((toCode letter) + number)
   else if ( (toCode letter) + number < 97) then  fromCode (122+(toCode letter)-number-97+1)  
   else   fromCode((toCode letter)-122+number+97-1)
 else 'A'

decode: Int -> Char -> Char 
decode number letter = encode -number letter    

encrypt: Int -> String -> String
encrypt number str =  String.fromList( List.map (encode number) (String.toList str))

decrypt: Int -> String -> String
decrypt number str = encrypt -number str
sqr : Int -> Int
sqr a=a^2

isTriple: Int -> Int -> Int -> Bool
isTriple a b c = 
   if ((sqr a) + (sqr b) == (sqr c)) then 
      True
  else 
      False

leg1 : Int -> Int -> Int 
leg1 x y = if x > y then (sqr x) - (sqr y)
 else (sqr y) - (sqr x) 

leg2 : Int -> Int -> Int 
leg2 x y = 2*x*y

hyp : Int -> Int -> Int
hyp x y = (sqr x) + (sqr y) 

pythTriple : (Int, Int) -> (Int, Int, Int)
pythTriple(x, y) = ((leg1 x y), (leg2 x y), (hyp x y))

isTripleTuple: (Int, Int, Int) -> Bool
isTripleTuple(a, b, c) = if ((isTriple a b c) == True) then True 
  else False


normalize: String -> String
normalize str =String.fromList(List.filter isAlpha (String.toList str))

 
pythTriplesMap: List (Int, Int) -> List (Int, Int, Int)
pythTriplesMap pyList = List.map pythTriple pyList
pythTriplesRec: List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec pyList = case pyList of
       head :: tail ->
           pythTriple head :: pythTriplesRec tail 
       [] ->
           []

arePythTriplesFilter: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesFilter tuple = List.filter  isTripleTuple tuple
arePythTriplesRec: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesRec pyTuple = case pyTuple of
        head :: tail ->
            if ( isTripleTuple head) then
                head :: arePythTriplesRec tail
            else 
                arePythTriplesRec tail
        []->
            []

repeatUntil: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil above100 double a = 
                   if (above100 a == False) then
                          repeatUntil above100 double (double a)
                   else 
                          a

above100: Int -> Bool 
above100 x = 
         x > 100 
double: Int -> Int 
double x = 
         x * 2


 
  
my_results: List String
my_results =
    [
        "-- Hello-Elm output --\n\n  assignment:",
        pr <| encode 2 'A',
        pr <| decode 2 'C',
        pr <| sqr 2 ,
        pr <| isTriple 3 4 5,
        pr <| pythTriple (3, 4),
        pr <| isTripleTuple (3, 4, 5),
        pr <| normalize "Hello, Fontys!",
        pr <| encrypt 7 (normalize "Hello, Fontys!"),
        pr <| decrypt 7 "OlssvMvuafz",
        pr <| pythTriplesMap [(5,4),(2,1),(35,7)],
        pr <| pythTriplesRec [(5,4),(2,1),(35,7)],
        pr <| arePythTriplesFilter [(1,2,3), (9,40,41), (3,4,5), (100,2,500)],
        pr <| repeatUntil above100 double 7,

        "\n-- end --"
    ] 
 

-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
page_width = 80

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)
  
