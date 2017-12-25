{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module TH
     ( chooseByIndices
     , TextShow (..)
     , deriveTextShow
     ) where

import           Language.Haskell.TH
import qualified Data.Text as T
import           Control.Monad

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices size indices = do
    names <- replicateM size (newName "var")
    let used = map (`elem` indices) [0..size-1]
    let names_used = zip names used
    lamE [tupP $ map createPat names_used] (tupE $ map (\i -> varE $ names !! i) indices)
  where
    createPat :: (Name, Bool) ->  Q Pat
    createPat (s, True)  = varP s
    createPat (_, False) = wildP


class TextShow a where
  textShow :: a -> T.Text

  default textShow :: Show a => a -> T.Text
  textShow = T.pack . show

instance TextShow Int
instance TextShow Char
instance TextShow Bool

deriveTextShow :: Name -> Q [Dec]
deriveTextShow name = do
    TyConI (DataD _ _ _ _ constructors _) <- reify name
    sequence [instanceD (return []) (conT ''TextShow `appT` conT name) [funD (mkName "textShow") (map clauseByCon constructors)]]
  where
    clauseByCon :: Con -> Q Clause
    clauseByCon (NormalC conName bangTypes) = do
        names <- replicateM (length bangTypes) (newName "p")
        let bn = nameBase conName
        clause [conP conName (map varP names)] (normalB [|T.concat [T.pack bn, T.pack "{", T.intercalate (T.pack ", ") $(listE (map (\n -> varE (mkName "textShow") `appE` varE n) names)), T.pack "}"] |]) []
    clauseByCon _ = fail "Only normal constructors supported"
