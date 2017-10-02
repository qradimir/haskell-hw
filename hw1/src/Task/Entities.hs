module Task.Entities ( Health (..)
                     , Armor (..)
                     , Braveness (..)
                     , Beast (..)
                     , Knight (..)
                     , Entity (..)
                     , FightResult (..)
                     , damage
                     , fight
                     ) where

newtype Health    = Health Int
newtype Armor     = Armor Int
newtype Braveness = Braveness Int

data Beast = Beast Health Armor

data Knight = Knight Health Armor Braveness

class Entity a where
  health    :: a -> Health
  armor     :: a -> Armor
  braveness :: a -> Braveness

instance Entity Beast where
  health    (Beast h _) = h
  armor     (Beast _ a) = a
  braveness _           = Braveness 5

instance Entity Knight where
  health    (Knight h _ _) = h
  armor     (Knight _ a _) = a
  braveness (Knight _ _ b) = b

data FightResult = FightResult { rounds :: Int
                               , fstRestHealth :: Health
                               , sndRestHealth :: Health
                               }

damage :: Health -> Armor -> Health
(Health h) `damage` (Armor a) = Health $ h - a

fight :: (Entity a, Entity b) => a -> b -> FightResult
fight l r = chooseBraveFight (braveness l) (braveness r) (armor l) (armor r) $ FightResult 0 (health l) (health r)

  where
    chooseBraveFight (Braveness fBrave) (Braveness sBrave) | fBrave < sBrave = sndFight
                                                           | fBrave > sBrave = fstFight
                                                           | otherwise = bothFight

    bothFight _ _ fr@(FightResult _ (Health 0) _) = fr
    bothFight _ _ fr@(FightResult _ _ (Health 0)) = fr
    bothFight fArmor sArmor (FightResult curRound fHealth sHealth)
       = bothFight fArmor sArmor $ FightResult (curRound + 2) (fHealth `damage` sArmor) (sHealth `damage` sArmor)

    sndFight _ _ fr@(FightResult _ (Health 0) _) = fr
    sndFight _ _ fr@(FightResult _ _ (Health 0)) = fr
    sndFight fArmor sArmor (FightResult curRound fHealth sHealth)
      = fstFight fArmor sArmor $ FightResult (curRound + 1) fHealth (sHealth `damage` fArmor)

    fstFight _ _ fr@(FightResult _ (Health 0) _) = fr
    fstFight _ _ fr@(FightResult _ _ (Health 0)) = fr
    fstFight fArmor sArmor (FightResult curRound fHealth sHealth)
      = sndFight fArmor sArmor $ FightResult (curRound + 1) (fHealth `damage` sArmor) sHealth
