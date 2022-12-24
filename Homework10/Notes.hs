

data Box a = Empty | Has a

instance (Eq a) => Eq (Box a) where
    Has x == Has y = x == y
    Empty == Empty = True
    _ == _ = False

data Blockchain = Cardano | Ethereum | Bitcoin

instance Eq Blockchain where
    Cardano == Cardano   = True
    Ethereum == Ethereum = True
    Bitcoin == Bitcoin   = True
    _ == _               = False

data PaymentMethod = Cash | Card | CC

instance Eq PaymentMethod where
    Cash == Cash = True
    Card == Card = True
    CC == CC     = True
    _ == _       = False

newtype Country = Country { countryName :: String}

class WeAccept a where
    weAccept :: a -> Bool

instance WeAccept PaymentMethod where
    weAccept x = case x of
        Cash -> False
        Card -> True
        CC   -> True

instance WeAccept Blockchain where
    weAccept x = case x of
        Cardano  -> True
        Ethereum -> False
        Bitcoin  -> True

instance WeAccept Country where
    weAccept x = case countryName x of
        "Mordor" -> False
        _       -> True

instance (WeAccept a) => WeAccept (Box a) where
    weAccept (Has x) = weAccept x
    weAccept Empty   = False

fancyFunction :: (WeAccept a) => a -> String
fancyFunction x = 
    if weAccept x
        then "Do something Fancy"
        else "Don't do it!"

