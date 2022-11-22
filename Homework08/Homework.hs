import GHC.IO.Exception (IOErrorType(ResourceExhausted))
-- This homework is around creating Haskell types that represent wines from over the world.

-- Question 1
-- Different wines are made from different grapes, there are around 10000 varieties over the world!
-- Create a type synonym called "Grape" for the different grape names as strings.
-- Additionally, use this type synonym for the grapes: "Sangiovese", "Cabernet-sauvignon", "Merlot" and "Garnacha".
type Wine = String

sangiovese :: Wine
sangiovese = "Sangiovese"

cabSav :: Wine
cabSav = "Cabernet-sauvignon"

merlot :: Wine
merlot = "Merlot"

garnacha :: Wine
garnacha = "Garnacha"

-- Question 2
-- The most famous regions that export wine are located in France, Italy and Spain.
-- Each of these countries is divided up in smaller regions.
-- These smaller regions are known for a certain style, for example the Champagne region in France
-- Create a type synonym called "Region" for wine region given their country and region as a tuple of strings.
-- Additionally, use this type synonym for the regions: Bordeaux in France, Tuscany in Italy and Rioja in Spain.

type Country = String
type Local = String
type Region = (Country, Local)

bordeaux :: Region
bordeaux = ("France", "Bordeaux")

tuscany :: Region
tuscany = ("Italy", "Tuscany")

rioja :: Region
rioja = ("Spain", "Rioja")


-- Question 3
-- A wine is either one of three kinds, these are red, white or rose wine.
-- Besides its kind, each wine also has a given alcohol level.
-- Create a data type called "Kind" that represents these three kinds, with each capturing the level of alcohol.
-- Additionally, use this data type for the examples: red wine with 14.5% alcohol, white wine with 13% alcohol 
-- and Rose wine with 12% alcohol.

data Kinds = Red | White | Rose 
type APV = String
type Kind = (Kinds, APV)

kind1 :: Kind
kind1 = (Red, "14.5%")

kind2 :: Kind
kind2 = (White, "13%")

kind3 :: Kind
kind3 = (Rose, "12%")

-- Question 4
-- In the world of wines, bottles display all of the above information for the consumer on its label.
-- Create a record type called "Label" that captures the grapes that are in a whine, the region its from,
-- and it's kind. Notice that some wines are a blended combination of multiple grapes!
-- Additionally, create for each of the described wine below a label.
data Bottle 
    = Bottle
        { label  :: String
        , grape  :: [String]
        , region :: (Country, Local)
        , kind   :: Kind  
        }
-- Larrosa Rose is a rose wine from the region Rioja. It is made from the Garnacha grape and 
-- has a alcohol level of 14%.
larrosaRose :: Bottle
larrosaRose = Bottle 
    { label  = "Larrosa Rose"
    , grape  = ["Garnacha"]
    , region = ("Spain","Rioja")
    , kind   = (Rose, "14%")
    }

-- Castiglioni is a red wine from the region of Tuscany. It is made from the grape Sangiovese and
-- has an alcohol level of 12.5%.
castiglioni :: Bottle
castiglioni = Bottle
    { label  = "Castiglioni"
    , grape  = ["Sangiovese"]
    , region = ("Italy", "Tuscany")
    , kind   = (Red, "12.5%")
    }

-- Bordeaux is known for its red wine, these are mainly a blend between Cabernet-sauvignon and Merlot.
-- Create a Label for the wine "Le Petit Haut Lafitte" that has an alcohol percentage 13.5%.
lePetitHautLafitte :: Bottle
lePetitHautLafitte = Bottle
    { label  = "le Petit Haut Lafitte"
    , grape  = ["Cabernet-sauvignon", "Merlot"]
    , region = ("France", "Bordeaux")
    , kind   = (Red, "13.5%")
    }

grapes :: Bottle -> [String]
grapes Bottle {grape=g} = g 

labels :: Bottle -> String
labels Bottle {label=l} = l


listGrapes :: [Bottle] -> [[String]]
listGrapes [] = []
listGrapes (Bottle {grape=g}:gs) = g : listGrapes gs


-- Question 5
-- Write a function `containsGrape` that takes a list of Labels and a Grape and returns a boolean.
-- The function should check if the there exists a wine in the Label that contains this Grape.

containsGrape :: String -> [Bottle] -> Bool
containsGrape _ [] = False
containsGrape x (Bottle {grape=g}:gs)
    | x `elem` g = True
    | otherwise  = containsGrape x gs


-- This is a test list for the `containsGrape` function with an grape that is not in the list.
grapeList = [larrosaRose,castiglioni,lePetitHautLafitte]
newGrape = "Pinot Noir"
