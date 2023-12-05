data Personne = Person { 
  nom :: String,
  prenom::String,
  adresse::String,
  email::String
} deriving Show

data Annuaire a =  Cons Personne ( Annuaire Personne ) | Nil

pers1 = Person{ 
  nom = "kenfack",
  prenom = "paul",
  adresse= "mada",
  email = "paulkenfack@gmail.com"  
}

pers2 = Person{ 
  nom = "fonfack",
  prenom = "enriette",
  adresse= "tule",
  email = "enriette@gmail.com"  
}

ann= Cons pers1 (Cons pers2 Nil)

search :: String -> Annuaire a -> Maybe String
search _ Nil = Nothing
search name (Cons p ps) = if (name == (nom p)) then Just (email p) 
                    else (search name ps)