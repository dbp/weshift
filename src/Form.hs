{-# LANGUAGE OverloadedStrings #-}
module Form where
  
import            Text.Digestive.Types
import            Text.Digestive.Blaze.Html5
import            Text.Digestive.Forms.Snap
import            Text.Digestive.Validate
import            Text.Digestive.Transform

import            Text.Blaze (Html)
import            Text.XmlHtml (docContent)
import            Text.Blaze.Renderer.XmlHtml (renderHtml)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            State
import            Control.Applicative
import            Control.Monad
import            Text.Templating.Heist

import            Application



