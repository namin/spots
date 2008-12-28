data Estimate a = Estimate a a deriving Eq

instance Show a => Show (Estimate a) where
    show (Estimate v e) = (show v) ++ " +- " ++ (show e)

instance Num a => Num (Estimate a) where
    (Estimate v1 e1) + (Estimate v2 e2) = Estimate (v1+v2) (e1+e2)
    (Estimate v1 e1) - (Estimate v2 e2) = Estimate (v1-v2) (e1+e2)
    (Estimate v1 e1) * (Estimate v2 e2) = Estimate (v1*v2) ((abs v1)*e2+(abs v2)*e1+e1*e2)
    abs (Estimate v e) = Estimate (abs v) e
    signum (Estimate v e) = Estimate (signum v) (signum (v+e) - signum (v-e))
    fromInteger n = Estimate (fromInteger n) 0

instance Fractional a => Fractional (Estimate a) where
    (Estimate v1 e1) / (Estimate v2 e2) = Estimate (v1/v2) ((av1*e2+av2*e1)/(av2*(av2-e2)))
                                          where av1 = abs v1
                                                av2 = abs v2
    fromRational n = Estimate (fromRational n) 0

(+-) v e = Estimate v e

--- (\x -> x+2*x+3*x*x)(1 +- 0.1)
-- | 6.0 +- 0.9300000000000002
--- (\x -> let y = x+x in y*y + 2)(1 +- 0.1)
-- | 6.0 +- 0.8400000000000001
poly x = x+2*x+3/(x*x)
--- poly(3.0 +- 0.1)
-- | 9.333333333333334 +- 0.32423520063567746
--- (\x -> poly (x*x)) (3.0 +- 1.0)
-- | 27.037037037037038 +- 20.93104806934594

