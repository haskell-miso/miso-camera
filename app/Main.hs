----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso hiding ((<#))
import           Miso.Media
import qualified Miso.Style as Style
----------------------------------------------------------------------------
import           Data.Function
import qualified Data.Map.Strict as M
----------------------------------------------------------------------------
-- | Component model state
data Action
  = OpenWebCam
  | OpenedWebCam Stream
  | ErrorWebCam JSVal
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run $ startApp app
  { events = M.insert "click" False mediaEvents
  }
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
type Model = ()
----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = component () update_ $ \() ->
  div_
  [ Style.style_
    [ Style.width (Style.px 500)
    , Style.height (Style.px 400)
    , Style.border "2px solid black"
    ]
  ]
  [ h2_ [] [ "🍜 miso-camera 📷" ]
  , video_
    [ id_ "vid"
    , muted_ True
    , Style.style_
      [ Style.width (Style.px 500)
      , Style.height (Style.px 400)
      , "object-fit" =: "cover"
      ]
    ]
    []
  , button_
    [ id_ "button"
    , autoplay_ True
    , onClick OpenWebCam 
    ]
    [ "Open WebCam"
    ]
  ] where
     update_ :: Action -> Effect Model Action
     update_ = \case
       OpenWebCam ->
         getUserMedia userMedia OpenedWebCam ErrorWebCam
       OpenedWebCam stream ->
         io_ $ do
           vid <- Media <$> getElementById "vid"
           vid & srcObject stream
           vid & play
       ErrorWebCam errorValue ->
         io_ $ do
           consoleLog "Error opening webcam"
           consoleLog' errorValue
----------------------------------------------------------------------------
