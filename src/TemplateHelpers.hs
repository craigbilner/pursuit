module TemplateHelpers where

import Import hiding (span)
import qualified Data.List.Split as List
import Data.Text (splitOn)
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html5 as H hiding (map, link)
import Text.Blaze.Html5.Attributes as A hiding (span, name, start)
import qualified Web.Bower.PackageMeta as Bower
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D

import Model.DocsAsHtml
import Model.DocLinks
import GithubAPI (ReadmeMissing(..))
import qualified GithubAPI

linkToGithubUser :: D.GithubUser -> Html
linkToGithubUser user =
  a ! href (toValue ("https://github.com/" <> D.runGithubUser user)) $ do
    toHtml (D.runGithubUser user)

linkToGithub :: (D.GithubUser, D.GithubRepo) -> Html
linkToGithub (user, repo) =
  let path = D.runGithubUser user <> "/" <> D.runGithubRepo repo
  in a ! href (toValue ("https://github.com/" <> path)) $ do
    toHtml path

joinLicenses :: [String] -> Maybe Html
joinLicenses ls
  | null ls   = Nothing
  | otherwise = Just (strong (toHtml (intercalate "/" ls)))

renderVersionRange :: Bower.VersionRange -> Html
renderVersionRange = toHtml . Bower.runVersionRange

linkToModule :: D.VerifiedPackage -> P.ModuleName -> Handler Html
linkToModule pkg mn =
  withUrlRenderer [hamlet|
    <a href=@{moduleDocsRoute pkg (P.runModuleName mn)}>#{insertBreaks mn}
    |]

renderModuleList :: D.VerifiedPackage -> Handler Html
renderModuleList pkg = do
  htmlRenderContext <- getHtmlRenderContext
  let docsOutput = packageAsHtml (htmlRenderContext pkg) pkg
      moduleNames = sort . map fst $ htmlModules docsOutput
  moduleLinks <- traverse (linkToModule pkg) moduleNames

  withUrlRenderer [hamlet|
    <ul .documentation-contents>
      $forall link <- moduleLinks
        <li>#{link}
    |]

-- | Insert <wbr> elements in between elements of a module name, in order to
-- prevent awkward line breaks or overflowing (and generating horizontal
-- scrollbars).
insertBreaks :: P.ModuleName -> Html
insertBreaks =
  P.runModuleName
   >>> splitOn "."
   >>> map toHtml
   >>> intercalate (toHtml ("." :: Text) *> wbr)

tryGetReadme :: D.VerifiedPackage -> Handler (Either ReadmeMissing Html)
tryGetReadme D.Package{..} = do
  mtoken <- appGithubAuthToken . appSettings <$> getYesod
  let (ghUser, ghRepo) = pkgGithub
  let ghTag = pkgVersionTag
  res <- GithubAPI.getReadme mtoken ghUser ghRepo ghTag
  case res of
    Left (OtherReason r) -> do
      $logError (tshow r)
    _ ->
      return ()
  return res

renderReadme :: Either ReadmeMissing Html -> Html
renderReadme = \case
  Right html' ->
    html'
  Left APIRateLimited ->
    [shamlet|
      <div .message .not-available>
        No readme available (due to rate limiting). Please try again later.
    |]
  Left ReadmeNotFound ->
    [shamlet|
      <div .message .not-available>
        No readme found in the repository at this tag. If you are the maintainer,
        perhaps consider adding one in the next release.
    |]
  Left (OtherReason _) ->
    [shamlet|
      <div .message .not-available>
        No readme available, for some unexpected reason (which has been logged).
        Perhaps
        <a href="https://github.com/purescript/pursuit/issues/new">
          open an issue?
    |]

renderHtmlDocs :: D.VerifiedPackage -> Text -> Handler (Maybe Html)
renderHtmlDocs pkg mnString = do
  htmlRenderContext <- getHtmlRenderContext
  let docsOutput = packageAsHtml (htmlRenderContext pkg) pkg
      mn = P.moduleNameFromString mnString
  traverse render $ lookup mn (htmlModules docsOutput)

  where
  render :: HtmlOutputModule LT.Text -> Handler Html
  render HtmlOutputModule{..} = do
    let locals = preEscapedToHtml htmlOutputModuleLocals
    reexports <- traverse renderReExports htmlOutputModuleReExports
    return (locals *> mconcat reexports)

  renderReExports :: (P.ModuleName, LT.Text) -> Handler Html
  renderReExports (mn, decls) = do
    moduleLink <- linkToModule pkg mn
    pure ((h2 ! class_ "re-exports" $
            (text "Re-exports from " *> strong moduleLink))
          *> preEscapedToHtml decls)

-- | Produce a Route for a given DocLink.
docLinkRoute :: LinksContext -> P.ModuleName -> DocLink -> Route App
docLinkRoute LinksContext{..} srcModule link = case linkLocation link of
  SameModule ->
    mkRoute ctxPackageName ctxVersion srcModule
  LocalModule _ otherModule ->
    mkRoute ctxPackageName ctxVersion otherModule
  DepsModule _ otherPackageName otherVersion otherModule ->
    mkRoute otherPackageName otherVersion otherModule
  where
  mkRoute pkgName version modName =
    PackageVersionModuleDocsR
      (PathPackageName pkgName)
      (PathVersion version)
      (P.runModuleName modName)

getHtmlRenderContext :: Handler (D.Package a -> P.ModuleName -> HtmlRenderContext)
getHtmlRenderContext = do
  renderUrl <- getUrlRender
  return $ \pkg currentMn ->
    let
      linksContext = getLinksContext pkg
    in
      HtmlRenderContext
        { currentModuleName = currentMn
        , buildDocLink = getLink linksContext currentMn
        , renderDocLink = renderUrl . docLinkRoute linksContext currentMn
        , renderSourceLink = renderSourceLink' linksContext
        }

renderSourceLink' :: LinksContext -> P.SourceSpan -> Text
renderSourceLink' LinksContext{..} (P.SourceSpan name start end) =
  concat
    [ githubBaseUrl
    , "/blob/"
    , ctxVersionTag
    , "/"
    , pack (relativeToBase (unpack name))
    , "#", fragment
    ]
  where
  (P.SourcePos startLine _) = start
  (P.SourcePos endLine _) = end
  (D.GithubUser user, D.GithubRepo repo) = ctxGithub

  relativeToBase = intercalate "/" . dropWhile (/= "src") . splitOnPathSep
  githubBaseUrl = concat ["https://github.com/", user, "/", repo]
  fragment = "L" <> tshow startLine <> "-L" <> tshow endLine

-- | Split a string on either unix-style "/" or Windows-style "\\" path
-- | separators.
splitOnPathSep :: String -> [String]
splitOnPathSep str
  | '/'  `elem` str = List.splitOn "/" str
  | '\\' `elem` str = List.splitOn "\\" str
  | otherwise       = [str]

-- | Render a URL together with a fragment (possibly).
getFragmentRender :: Handler ((Route App, Maybe Text) -> Text)
getFragmentRender = do
  render <- getUrlRender
  return $ \(route, fragment) -> render route ++ maybe "" ("#" ++) fragment
