module Builder
     ( ProjectSettings (..)
     , Project (..)
     , ProjectBuilder
     , buildProject
     , benchs
     , github
     , travis
     ) where

import Data.Text
import Control.Comonad.Traced

data ProjectSettings = ProjectSettings
     { settingsBenchs :: Bool  -- ^ enable benchmarks for project?
     , settingsGithub :: Bool  -- ^ set up github     for project?
     , settingsTravis :: Bool  -- ^ set up Travis CI  for project?
     }

instance Monoid ProjectSettings where
  mempty = ProjectSettings False False False

  mappend ps1 ps2 = let sBenchs =  settingsBenchs ps1 || settingsBenchs ps2 in
                    let sGithub =  settingsGithub ps1 || settingsGithub ps2 in
                    let sTravis =  settingsTravis ps1 || settingsTravis ps2 in
                    ProjectSettings sBenchs sGithub sTravis

--      extend :: ((m -> a) -> b) -> (m -> a) -> m -> b
--      extend f ma m = f $ \m' -> ma (m <> m')

-- =>> :: (m -> a) -> ((m -> a) -> a) -> m -> a
-- ma =>> f = \m -> f $ \m' -> ma (m <> m')

-- ma =>> f =>> g = \m -> g $ \m' -> (ma =>> f) (m <> m')
--                = \m -> g $ \m' -> (f $ \m'' -> ma ((m <> m') <> m''))

data Project = Project
     { projectName :: Text
     , hasBenchs   :: Bool
     , hasGithub   :: Bool
     , hasTravis   :: Bool
     } deriving (Show)

type ProjectBuilder = Traced ProjectSettings Project

buildProject :: Text -> ProjectBuilder
buildProject name = traced $ \ps -> Project name (settingsBenchs ps) (settingsGithub ps) (settingsTravis ps)


benchs :: ProjectBuilder -> Project
benchs builder = runTraced builder $ ProjectSettings True False False

github :: ProjectBuilder -> Project
github builder = runTraced builder $ ProjectSettings False True False

travis :: ProjectBuilder -> Project
travis builder = let pr = runTraced builder $ ProjectSettings False False True
                 in if hasGithub pr
                   then pr
                   else pr {hasTravis = False}
