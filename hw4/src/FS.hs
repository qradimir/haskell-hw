{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module FS
     ( FS(..)
     , scanDir
     , contents
     , name
     , cd
     , ls
     , file
     , renameExt
     , allNames
     ) where

import Control.Lens
import Control.Monad     (forM)
import System.Directory  (getDirectoryContents, doesDirectoryExist)
import System.FilePath   ((</>), takeDirectory, replaceExtension)

data FS
    = Dir
          { _name     :: FilePath  -- название папки, не полный путь
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath  -- название файла, не полный путь
          }
  deriving (Eq, Show)

scanDir :: FilePath -> IO FS
scanDir p = Dir (takeDirectory p) <$> getChildren p
  where
    getChildren path = do
          cs <- filter (`notElem` [".",".."]) <$> getDirectoryContents path
          forM cs $ \c -> do
            let c' = path </> c
            isDir <- doesDirectoryExist c'
            if isDir
              then fmap (Dir c) . getChildren $ c'
              else return $ File c

makeLenses ''FS
makePrisms ''FS

cd :: String -> Traversal' FS FS
cd s = contents.traversed.filtered isMyDir
 where
   isMyDir :: FS -> Bool
   isMyDir (Dir n _) | n == s = True
   isMyDir _                  = False

ls :: Traversal' FS String
ls = contents.traversed.name

file :: String -> Traversal' FS String
file s = contents.traversed.filtered isMyFile.name
  where
    isMyFile :: FS -> Bool
    isMyFile (File n) | n == s = True
    isMyFile _                 = False

renameExt :: String -> FS -> FS
renameExt newExt f = f & contents.traversed._File %~ replaceExtension newExt

allNames :: FS -> [String]
allNames f = f^.name : concatMap allNames (f^.contents)
