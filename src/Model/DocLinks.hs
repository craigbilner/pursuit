
module Model.DocLinks where

import Prelude
import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Version
import Data.Text (Text)

import Web.Bower.PackageMeta hiding (Version)

import qualified Language.PureScript as P

import Language.PureScript.Docs.Types

data LinksContext = LinksContext
  { ctxGithub               :: (GithubUser, GithubRepo)
  , ctxModuleMap            :: Map P.ModuleName PackageName
  , ctxResolvedDependencies :: [(PackageName, Version)]
  , ctxPackageName          :: PackageName
  , ctxVersion              :: Version
  , ctxVersionTag           :: Text
  }
  deriving (Show, Eq, Ord)

data DocLink = DocLink
  { linkLocation  :: LinkLocation
  , linkTitle     :: Text
  , linkNamespace :: Namespace
  }
  deriving (Show, Eq, Ord)

data LinkLocation
  -- | A link to a declaration in the same module.
  = SameModule

  -- | A link to a declaration in a different module, but still in the current
  -- package; we need to store the current module and the other declaration's
  -- module.
  | LocalModule P.ModuleName P.ModuleName

  -- | A link to a declaration in a different package. We store: current module
  -- name, name of the other package, version of the other package, and name of
  -- the module in the other package that the declaration is in.
  | DepsModule P.ModuleName PackageName Version P.ModuleName

  -- | A link to a declaration that is built in to the compiler, e.g. the Prim
  -- module. In this case we only need to store the module that the builtin
  -- comes from (at the time of writing, this will only ever be "Prim").
  | BuiltinModule P.ModuleName
  deriving (Show, Eq, Ord)

-- | Given a links context, a thing to link to (either a value or a type), and
-- its containing module, attempt to create a DocLink.
getLink :: LinksContext -> P.ModuleName -> Namespace -> Text -> ContainingModule -> Maybe DocLink
getLink LinksContext{..} curMn namespace target containingMod = do
  location <- getLinkLocation
  return DocLink
    { linkLocation = location
    , linkTitle = target
    , linkNamespace = namespace
    }

  where
  getLinkLocation = normalLinkLocation <|> builtinLinkLocation

  normalLinkLocation = do
    case containingMod of
      ThisModule ->
        return SameModule
      OtherModule destMn ->
        case Map.lookup destMn ctxModuleMap of
          Nothing ->
            return $ LocalModule curMn destMn
          Just pkgName -> do
            pkgVersion <- lookup pkgName ctxResolvedDependencies
            return $ DepsModule curMn pkgName pkgVersion destMn

  builtinLinkLocation = do
    let primMn = P.moduleNameFromString "Prim"
    guard $ containingMod == OtherModule primMn
    -- TODO: ensure the declaration exists in the builtin module too
    return $ BuiltinModule primMn

getLinksContext :: Package a -> LinksContext
getLinksContext Package{..} =
  LinksContext
    { ctxGithub               = pkgGithub
    , ctxModuleMap            = pkgModuleMap
    , ctxResolvedDependencies = pkgResolvedDependencies
    , ctxPackageName          = bowerName pkgMeta
    , ctxVersion              = pkgVersion
    , ctxVersionTag           = pkgVersionTag
    }
