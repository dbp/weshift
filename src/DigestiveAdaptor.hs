{-# LANGUAGE OverloadedStrings #-}

module DigestiveAdaptor where

-- this module defines default splices for the ones that digestive-functors-heist
-- gives for a view. this is so that forms can be rendered without a call to
-- digestiveSplices with the appropriate view.

import           Control.Monad      (liftM, mplus)
import           Data.Function      (on)
import           Data.List          (unionBy)
import           Data.Maybe         (fromJust, fromMaybe, maybeToList)
import           Data.Monoid        (mappend)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml       as X

import           Application
import           Snap.Snaplet.Heist

import           Debug.Trace        (trace)

--------------------------------------------------------------------------------
digestiveAdaptorSplices :: Splices (Splice AppHandler)
digestiveAdaptorSplices = do "dfInput" ## dfInput
                             "dfInputText" ## dfInputText
                             "dfInputTextArea" ## dfInputTextArea
                             "dfInputPassword" ## dfInputPassword
                             "dfInputHidden" ## dfInputHidden
                             "dfInputCheckbox" ## dfInputCheckbox
                             "dfInputFile" ## dfInputFile
                             "dfInputSubmit" ## dfInputSubmit
                             "dfLabel" ## dfLabel
                             "wsSelect" ## wsSelect
                             "dfForm" ## dfForm
                             "dfErrorList" ## dfErrorList
                             "dfChildErrorList" ## dfChildErrorList


--------------------------------------------------------------------------------
attr :: Bool -> (Text, Text) -> [(Text, Text)] -> [(Text, Text)]
attr False _ = id
attr True a = (a :)


--------------------------------------------------------------------------------
makeElement :: Text -> [X.Node] -> [(Text, Text)] -> [X.Node]
makeElement name nodes = return . flip (X.Element name) nodes


--------------------------------------------------------------------------------
getDefaultValue :: [(Text, Text)] -> [(Text, Text)]
getDefaultValue as = maybeToList $ do
    v <- lookup "data-default" as
    return ("value", v)

--------------------------------------------------------------------------------
getRefAttributes :: Monad m
                 => Maybe Text -- ^ Optional default ref
                 -> HeistT m m (Text, [(Text, Text)]) -- ^ (Ref, other attrs)
getRefAttributes defaultRef = do
    node <- getParamNode
    return $ case node of
        X.Element _ as _ ->
            let ref = fromMaybe (error $ show node ++ ": missing ref") $
                        lookup "ref" as `mplus` defaultRef
            in (ref, filter ((/= "ref") . fst) as)
        _ -> (error "Wrong type of node!", [])


--------------------------------------------------------------------------------
getContent :: Monad m => HeistT m m [X.Node]
getContent = liftM X.childNodes getParamNode


--------------------------------------------------------------------------------
-- | Does not override existing attributes
addAttrs :: [(Text, Text)] -- ^ Original attributes
         -> [(Text, Text)] -- ^ Attributes to add
         -> [(Text, Text)] -- ^ Resulting attributes
addAttrs = unionBy (on (==) fst)


dfInput :: Monad m => Splice m
dfInput = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref' = T.append "ws." ref
    return $ makeElement "input" [] $ addAttrs attrs
        [("id", ref'), ("name", ref')]

dfInputText :: Monad m => Splice m
dfInputText = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref' = T.append "ws." ref
    let def = getDefaultValue attrs
    return $ makeElement "input" [] $ addAttrs attrs
        ([("type", "text"), ("id", ref'), ("name", ref')] ++ def)

dfInputTextArea :: Monad m => Splice m
dfInputTextArea = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref' = T.append "ws." ref
    return $ makeElement "textarea" [] $ addAttrs attrs
        [("id", ref'), ("name", ref')]

dfInputPassword :: Monad m => Splice m
dfInputPassword = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref' = T.append "ws." ref
    return $ makeElement "input" [] $ addAttrs attrs
        [("type", "password"), ("id", ref'), ("name", ref')]

dfInputHidden :: Monad m => Splice m
dfInputHidden = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref' = T.append "ws." ref
    let def = getDefaultValue attrs
    return $ makeElement "input" [] $ addAttrs attrs
        ([("type", "hidden"), ("id", ref'), ("name", ref')] ++ def)

-- can't be done
--dfInputSelect :: Monad m => Splice m
--dfInputRadio :: Monad m => Splice m

dfInputCheckbox :: Monad m => Splice m
dfInputCheckbox = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref' = T.append "ws." ref
    return $ makeElement "input" [] $ addAttrs attrs $
        [("type", "checkbox"), ("id", ref'), ("name", ref')]

dfInputFile :: Monad m => Splice m
dfInputFile = do
    (ref, attrs) <- getRefAttributes Nothing
    let ref' = T.append "ws." ref
    return $ makeElement "input" [] $ addAttrs attrs
        [("type", "file"), ("id", ref'), ("name", ref')]


dfInputSubmit :: Monad m => Splice m
dfInputSubmit = do
    (_, attrs) <- getRefAttributes Nothing
    return $ makeElement "input" [] $ addAttrs attrs [("type", "submit")]

dfLabel :: Monad m => Splice m
dfLabel = do
    (ref, attrs) <- getRefAttributes Nothing
    content <- getContent
    let ref' = T.append "ws." ref
    return $ makeElement "label" content $ addAttrs attrs [("for", ref')]

wsSelect :: Monad m => Splice m
wsSelect = do
    node <- getParamNode
    case node of
        X.Element _ as chlds -> do
            let dval = fromMaybe "" $ lookup "data-default" as
            let name = fromJust $ lookup "name" as
            return $ [X.Element "select" (addAttrs [("name", T.append "ws." name)] as)
                                        (map (cld dval) chlds)]
        _ -> return []
  where cld def e = case e of
            (X.Element n as cs) -> if ((lookup "value" as) == (Just def)) then
                                      (X.Element n (addAttrs [("selected", "selected")] as) cs)
                                   else e
            _ -> e -- should not happen, all children should be elements

dfForm :: Monad m => Splice m
dfForm = do
    (_, attrs) <- getRefAttributes Nothing
    content <- getContent
    return $ makeElement "form" content $ addAttrs attrs
        [ ("method", "POST")
        ]

dfErrorList :: Monad m => Splice m
dfErrorList = return []

dfChildErrorList :: Monad m => Splice m
dfChildErrorList = return []
