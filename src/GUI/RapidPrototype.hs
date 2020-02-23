-- | quick prototype REPL.

module GUI.RapidPrototype where

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

import Runtime.Eval
import Data.Array
import Data.List

import Runtime.Typechecker
import Control.Monad
import Language.Syntax
import Runtime.Repl
import Parser.Parser
import Data.List.Extra
import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan

-- | run an interactive game, with graphics

runPrototype :: String -> IO ()
runPrototype f = do
  Just g <- parseGameFile f
  True <- tc g
  replIn <- Chan.newChan
  startGUI defaultConfig
    { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } $ prototype g replIn


prototype :: Game -> Chan String -> Window -> UI ()
prototype g@(Game n i b _) replIn window = do
  return window # set UI.title n
  inputarea <- makeinput replIn
  getBody window #+ [element inputarea]
  replyReciever <- liftIO $ forkIO $ replReply g window replIn inputarea
  return ()


replReply :: Game -> Window -> Chan String -> Element -> IO ()
replReply g@(Game n i@(BoardDef szx szy p) b vs) w msgs replArea = do
  reply <- Chan.getChanContents msgs
  forM_ reply $ \msg ->
    do
      runUI w $ do
        case parseLine msg of
          Right x -> do
            case tcexpr (environment i b vs) x of
              Right t -> do
                case runWithTape (bindings (szx, szy) vs) [] x of
                  Right (x) -> element replArea #+ (pure $ makeValDisplay x msg)
                  Left ((Vboard b), t) -> do
                    element replArea #+ [UI.div #. "content" #+ [makeInteractiveBoard b t x msg]]
                  Left (err) -> string $ show err
                UI.scrollToBottom replArea
                flushCallBuffer
              Left err -> do
                element replArea #+ (pure $ (string $ show err))
                UI.scrollToBottom replArea
                flushCallBuffer


          Left err -> do
            element replArea #+ [UI.div #. "repl" #+ [UI.div #. "inprogress" #+ [string $ show err]]]
            UI.scrollToBottom replArea
            flushCallBuffer
    where
      makeInteractiveBoard :: Array (Int,Int) Val -> [Val] -> Expr -> String -> UI Element
      makeInteractiveBoard arr t ex msg = do
        UI.div #. "board" #+ (map (\r -> UI.div #. "row" #+ r) $ (flip map) (toGrid arr) $ \row -> (flip map) row (\cell -> do
          b <- UI.button #. "click-cell" #+ [string ((show . snd) cell)]
          on UI.click b $ \_ -> do
            case runWithTape (bindings (szx, szy) vs) (t ++ (pure $ Vpos (fst cell))) ex of
              Right x -> element replArea #+ (pure $ makeValDisplay x msg)
              Left ((Vboard b), t') -> element replArea #+ [UI.div #. "inprogress" #+ [makeInteractiveBoard b t' ex msg]]
              Left err -> element replArea #+ (pure $ string $ show err)
          return b))
      makeValDisplay x msg =
        UI.div #. "repl" #+ [UI.div #. "content" #+ [string ("> " ++ msg)]] #+ [UI.div #. "content" #+ [case x of
                                                                                                (Vboard b) -> makeBoard b
                                                                                                x -> string $ show x]]


makeinput :: Chan String -> UI Element
makeinput i = do
  in_ <- UI.textarea #. "send-textarea"
  on UI.sendValue in_ $ (. trim) $ \content -> do
        element in_ # set value ""
        when (not (null content)) $ liftIO $ do
          Chan.writeChan i (content)
  UI.div #. "message-area" #+ [UI.div #. "send-area" #+ [element in_]]



makeBoard arr = UI.div #. "boardState" #+ map (\x -> UI.div #. "cell" #+ [(string $ concatMap (show . snd) x)]) (groupBy (\x y -> (fst . fst) x == (fst . fst) y) (assocs arr))






toGrid x = (groupBy (\x y -> (fst . fst) x == (fst . fst) y) (assocs x))
