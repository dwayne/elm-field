module Lib.Selection.NonEmpty exposing (Selection, fromList, select, toList)


type Selection a
    = Selection (List a) a (List a)


fromList : List a -> a -> List a -> Selection a
fromList =
    Selection


select : a -> Selection a -> Selection a
select x ((Selection before selected after) as selection) =
    case splitAt x (List.concat [ before, [ selected ], after ]) of
        Just ( l, r ) ->
            Selection l x r

        Nothing ->
            selection


splitAt : a -> List a -> Maybe ( List a, List a )
splitAt =
    splitAtHelper []


splitAtHelper : List a -> a -> List a -> Maybe ( List a, List a )
splitAtHelper revBefore item list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if item == x then
                Just ( List.reverse revBefore, xs )

            else
                splitAtHelper (x :: revBefore) item xs


toList :
    { onSelected : a -> b
    , onOther : a -> b
    }
    -> Selection a
    -> List b
toList { onSelected, onOther } (Selection before selected after) =
    List.concat
        [ List.map onOther before
        , [ onSelected selected ]
        , List.map onOther after
        ]
