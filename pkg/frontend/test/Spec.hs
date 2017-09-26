{-# LINE 1 "test/" #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Spec where
import qualified Refine.Frontend.AccessSpec
import qualified Refine.Frontend.Contribution.BubbleSpec
import qualified Refine.Frontend.Contribution.DialogSpec
import qualified Refine.Frontend.Contribution.DiscussionSpec
import qualified Refine.Frontend.Contribution.QuickCreateSpec
import qualified Refine.Frontend.Document.DocumentSpec
import qualified Refine.Frontend.Header.DocumentHeaderSpec
import qualified Refine.Frontend.Header.HeadingSpec
import qualified Refine.Frontend.Header.ToolbarSpec
import qualified Refine.Frontend.IconSpec
import qualified Refine.Frontend.MainMenuSpec
import qualified Refine.Frontend.OrphansSpec
import qualified Refine.Frontend.RouteSpec
import qualified Refine.Frontend.StoreSpec
import qualified Refine.Frontend.ThirdPartyViewsSpec
import qualified Refine.Frontend.UtilSpec
import qualified Refine.Frontend.ViewsSpec
import Test.Hspec.Discover
main :: IO ()
main = hspec spec
spec :: Spec
spec = postProcessSpec "test/Refine/Frontend/AccessSpec.hs" (describe "Refine.Frontend.Access" Refine.Frontend.AccessSpec.spec) >> postProcessSpec "test/Refine/Frontend/Contribution/BubbleSpec.hs" (describe "Refine.Frontend.Contribution.Bubble" Refine.Frontend.Contribution.BubbleSpec.spec) >> postProcessSpec "test/Refine/Frontend/Contribution/DialogSpec.hs" (describe "Refine.Frontend.Contribution.Dialog" Refine.Frontend.Contribution.DialogSpec.spec) >> postProcessSpec "test/Refine/Frontend/Contribution/DiscussionSpec.hs" (describe "Refine.Frontend.Contribution.Discussion" Refine.Frontend.Contribution.DiscussionSpec.spec) >> postProcessSpec "test/Refine/Frontend/Contribution/QuickCreateSpec.hs" (describe "Refine.Frontend.Contribution.QuickCreate" Refine.Frontend.Contribution.QuickCreateSpec.spec) >> postProcessSpec "test/Refine/Frontend/Document/DocumentSpec.hs" (describe "Refine.Frontend.Document.Document" Refine.Frontend.Document.DocumentSpec.spec) >> postProcessSpec "test/Refine/Frontend/Header/DocumentHeaderSpec.hs" (describe "Refine.Frontend.Header.DocumentHeader" Refine.Frontend.Header.DocumentHeaderSpec.spec) >> postProcessSpec "test/Refine/Frontend/Header/HeadingSpec.hs" (describe "Refine.Frontend.Header.Heading" Refine.Frontend.Header.HeadingSpec.spec) >> postProcessSpec "test/Refine/Frontend/Header/ToolbarSpec.hs" (describe "Refine.Frontend.Header.Toolbar" Refine.Frontend.Header.ToolbarSpec.spec) >> postProcessSpec "test/Refine/Frontend/IconSpec.hs" (describe "Refine.Frontend.Icon" Refine.Frontend.IconSpec.spec) >> postProcessSpec "test/Refine/Frontend/MainMenuSpec.hs" (describe "Refine.Frontend.MainMenu" Refine.Frontend.MainMenuSpec.spec) >> postProcessSpec "test/Refine/Frontend/OrphansSpec.hs" (describe "Refine.Frontend.Orphans" Refine.Frontend.OrphansSpec.spec) >> postProcessSpec "test/Refine/Frontend/RouteSpec.hs" (describe "Refine.Frontend.Route" Refine.Frontend.RouteSpec.spec) >> postProcessSpec "test/Refine/Frontend/StoreSpec.hs" (describe "Refine.Frontend.Store" Refine.Frontend.StoreSpec.spec) >> postProcessSpec "test/Refine/Frontend/ThirdPartyViewsSpec.hs" (describe "Refine.Frontend.ThirdPartyViews" Refine.Frontend.ThirdPartyViewsSpec.spec) >> postProcessSpec "test/Refine/Frontend/UtilSpec.hs" (describe "Refine.Frontend.Util" Refine.Frontend.UtilSpec.spec) >> postProcessSpec "test/Refine/Frontend/ViewsSpec.hs" (describe "Refine.Frontend.Views" Refine.Frontend.ViewsSpec.spec)
