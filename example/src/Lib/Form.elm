module Lib.Form exposing
    ( Config
    , Form
    , isInvalid
    , isValid
    , new
    , toFields
    , update
    , validate
    , validateAsMaybe
    , validateAsResult
    )

import Validation as V exposing (Validation)


type Form fields setters error output
    = Form
        { config : Config fields setters error output
        , fields : fields
        }


type alias Config fields setters error output =
    { init : fields
    , setters : setters
    , validate : fields -> Validation error output
    }


new : Config fields setters error output -> Form fields setters error output
new config =
    Form
        { config = config
        , fields = config.init
        }


update : (setters -> a -> fields -> fields) -> a -> Form fields setters error output -> Form fields setters error output
update f x (Form form) =
    Form { form | fields = f form.config.setters x form.fields }


isValid : Form fields setters error output -> Bool
isValid =
    validate >> V.isValid


isInvalid : Form fields setters error output -> Bool
isInvalid =
    not << isValid


validate : Form fields setters error output -> Validation error output
validate (Form { config, fields }) =
    config.validate fields


validateAsResult : Form fields setters error output -> Result (List error) output
validateAsResult =
    validate >> V.toResult


validateAsMaybe : Form fields setters error output -> Maybe output
validateAsMaybe =
    validate >> V.toMaybe


toFields : Form fields setters error output -> fields
toFields (Form { fields }) =
    fields
