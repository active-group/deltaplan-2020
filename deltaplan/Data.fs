namespace Data

module Examples =
  // Ein Haustier ist *eins der folgenden:*
  // - Hund ODER
  // - Katze ODER
  // - Schlange
  // Fallunterscheidung / gemischten Daten
  // F#: algebraischer Datentyp
  // algebraic data type
  type Pet = Hund | Katze | Schlange

  // Ist Haustier niedlich?
  let isCute (pet: Pet): bool =
    // Verzweigung
    match pet with
    | Hund -> true
    | Katze -> true
    | Schlange -> false

  // Eine Uhrzeit besteht aus: / hat folgende Eigenschaften:
  // - Stunde UND
  // - Minute
  // Zusammengesetzten Daten
  // F#: Record-Typ
  type Time = { hour: int
                minute: int }


  let time1 = { hour = 12; minute = 24 }
  let time2 = { hour = 11; minute = 3 }

  // Minuten seit Mitternacht
  let msm (time: Time): int =
    time.hour * 60 + time.minute

  let msm' ({ hour = h; minute = m } as time): int =
    h * 60 + time.minute

  type Liveness = Dead | Alive

  type DilloType = { alive: Liveness; weight: int }

  let dt1: DilloType = { alive = Alive; weight = 12 }

  type SnakeType = { length: int; thickness: int }

  // Ein Tier ist eins der folgenden:
  // - ein Gürteltier
  // - eine Klapperschlange

  // Ein Gürteltier hat folgende Eigenschaften:
  // - tot oder lebendig
  // - Gewicht

  // Eine Klapperschlange hat folgende Eigenschaften:
  // - Länge
  // - Dicke


  // algebraischer Datentyp
  type Animal =
    // gemischte Daten: Jeder Fall braucht Konstruktor
    | Dillo of Liveness * int
    | Snake of SnakeType

  // Gürteltier, lebendig, 12kg
  let animal1: Animal = Dillo (Alive, 12)
  // Klapperschlange, Länge 3m, Dicke 10cm
  let animal2 = Snake { length = 300; thickness = 10 }

  // Tier überfahren
  let runOverAnimal (animal: Animal): Animal =
    match animal with
    | Dillo (_, weight) ->
        Dillo (Dead, weight)
    | Snake { length = l } ->
        Snake { length = l; thickness = 0 }


  // F#: nur 1stellige Funktionen
  // val feedAnimal : int -> (Animal -> Animal)
  let feedAnimal (amount: int) (animal: Animal): Animal =
    match animal with
    | Dillo (Alive, weight) ->
        Dillo (Alive, weight + amount)
    | Dillo (Dead, weight) -> animal
    | Snake { length = l; thickness = t } ->
        Snake { length = l; thickness = t + amount }

  let feedAnimal1 = feedAnimal 1
  let animal1' = animal1 |> feedAnimal 1

  // let feedAnimal' (animal: Animal) (amount: int): Animal =
  // feedAnimal amount animal

  // 'a: Typvariable
  // val flip :: ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
  let flip g b a = g a b

  let feedAnimal' = flip feedAnimal

  // val feedAnimal2 :: int * Animal -> Animal
  let feedAnimal2 (amount, animal) =
    match animal with
    | Dillo (Alive, weight) ->
        Dillo (Alive, weight + amount)
    | Dillo (Dead, weight) -> animal
    | Snake { length = l; thickness = t } ->
        Snake { length = l; thickness = t + amount }


  // Idee: Moses Schönfinkel
  //       Haskell Curry

  // val uncurry: ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
  let uncurry f (a, b) = f a b
  // val curry: ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
  // schönfinkeln
  // currifizieren
  let curry f a  b  = f (a, b)


  // feedAnimal2(1, animal1) 

  // Eine Liste ist eins der folgenden:
  // - die leere Liste
  // - eine Cons-Liste,
  //   bestehend aus erstem Element und Rest-Liste
  //                             Selbstbezug ^^^^^
  type List<'T> =
  | Empty
  | Cons of 'T * List<'T>

  type ListN = List<int>

  // 2elementige Liste: 1 2
  let listn2 = Cons (1, Cons (2, Empty))
  // 3elementige Liste: 3 5 7
  let listn3 = Cons (3, Cons (5, Cons (7, Empty)))

  let rec listSum'' (list: List<int>): int =
    match list with
    | Empty -> 0
    | Cons (first, rest) ->
        first + (listSum'' rest)

  // Elemente einer Liste addieren
  let rec listSum (list: list<int>): int =
     match list with
     | [] -> 0  // neutrales Element bezüglich +
     | (first::rest) ->
       (+) first // 1. Element
           (listSum rest) // Summe der restlichen Elemente

  // Elemente einer Liste multiplizieren
  let rec listProduct (list: list<int>): int =
     match list with
     | [] -> 1
     | (first::rest) ->
       (*) first
           (listProduct rest)

  // fold
  let rec listCalc (n: 'b) (f: 'a -> 'b -> 'b) (list: list<'a>): 'b =
     match list with
     | [] -> n
     | (first::rest) ->
       f first
         (listCalc n f rest)


  // Ist eine Zahl gerade?
  let isEven = fun n -> n % 2 = 0

  let rec listEvens (list: ListN): ListN =
    match list with
    | Empty -> Empty
    | Cons (first, rest) ->
      if isEven first
      then Cons (first, listEvens rest)
      else listEvens rest

  let isPositive = fun n -> n > 0
                // lambda
  let rec listPositives (list: ListN): ListN =
    match list with
    | Empty -> Empty
    | Cons (first, rest) ->
      if isPositive first
      then Cons (first, listPositives rest)
      else listPositives rest

  let rec listExtract (is: 'a -> bool) (list: List<'a>): List<'a> =
    match list with
    | Empty -> Empty
    | Cons (first, rest) ->
      if is first
      then Cons (first, listExtract is rest)
      else listExtract is rest

  let highway': List<Animal> = Cons (animal1, Cons (animal2, Empty))

  let listAliveDillos (list: List<Animal>): List<Animal> =
    let isAliveDillo animal =
      match animal with
      | Dillo (Alive, _) -> true
      | _ -> false
    listExtract isAliveDillo list


  let rec listSum' (list: list<int>): int =
    match list with
    | [] -> 0
    | (first::rest) ->
      first
      + listSum' rest


  let highway = [animal1; animal2]

  let rec runOverAnimals (list: list<Animal>): list<Animal> =
    match list with
    | [] -> []
    | (first::rest) ->
        (runOverAnimal first)
          :: (runOverAnimals rest)

  let rec feedAnimals amount (list: list<Animal>): list<Animal> =
    match list with
    | [] -> []
    | (first::rest) ->
        ((feedAnimal amount) first)
         :: (feedAnimals amount rest)

  // val forEach :: ('a -> 'b) -> (list<'a> -> list<'b>)
  let rec forEach (f: 'a -> 'b) (list: list<'a>): list<'b> =
    match list with
    | [] -> []
    | (first::rest) ->
        (f first)
          :: (forEach f rest)



  // 3 + 2 + 1
  // 1 + 2 + 3 + ... + 98 + 99 + 100
  // = (n + 1)*(n/2)
  // = (n * (n + 1)) / 2
  // = O(n^2)

  // appendToEnd [1;2;3] 4
  let rec appendToEnd (list: list<'a>) (x: 'a): list<'a> =
   match list with
   | [] -> [x]
   | (first::rest) ->
     first ::   // 1
       (appendToEnd rest x) // [2;3;4]

  // Liste umdrehen
  // rev [1;2;3] = [3;2;1]
  let rec rev (list: list<'a>): list<'a> =
    match list with
    | [] -> []
    | (first::rest) -> // first = 1, rest = [2, 3]
        appendToEnd (rev rest) // [3, 2]
                    first

  // acc: List der schon gesehenen Elemente, umgedreht
  let rec revHelper (list: list<'a>) (acc: list<'a>): list<'a> =
    match list with
    | [] -> acc
    | (first::rest) ->
        // endrekursiver Aufruf, tail call
        // verbraucht keinen Platz auf dem Stack
        revHelper rest (first::acc)


  let rev' (list: list<'a>): list<'a> =
    revHelper list []


  type Map<'key, 'value> = Map of list<'key * 'value>

  let emptyMap = Map []

  // Typ vorher:
  // 'key -> 'value -> list<'key * 'value> -> list<'key * 'value>

  let addToMap key value (Map map) = Map ((key, value)::map)

  (*
  type option<'a> = None | Some of 'a
  *)

  let rec lookupMap (key: 'key) (Map map: Map<'key, 'value>): option<'value> =
    match map with
    | [] -> None
    | ((key', value')::rest) ->
        match key' with
        | key -> Some value'
        | _-> lookupMap key (Map rest)

//        if key = key'
//        then Some value'
//        else lookupMap key (Map rest)


