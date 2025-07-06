module Lib.Form exposing (Config, Form, new, set, submit, toFields)


type Form name fields error output
    = Form
        { config : Config name fields error output
        , fields : fields
        }


type alias Config name fields error output =
    { init : fields
    , set : name -> String -> fields -> fields
    , submit : fields -> Result (List error) output
    }


new : Config name fields error output -> Form name fields error output
new config =
    Form
        { config = config
        , fields = config.init
        }


set : name -> String -> Form name fields error output -> Form name fields error output
set name s (Form { config, fields }) =
    Form
        { config = config
        , fields = config.set name s fields
        }


submit : Form name fields error output -> Result (List error) output
submit (Form { config, fields }) =
    config.submit fields


toFields : Form name fields error output -> fields
toFields (Form { fields }) =
    fields
