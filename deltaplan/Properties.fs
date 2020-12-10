﻿namespace Properties

module Examples =
  open FsCheck
  open FsCheck.Util

  let rev list0 =
    let rec loop list reversed =
        match list with
        | [] -> reversed
        | (first::rest) -> loop rest (first::reversed)
    loop list0 []

  // let rev list = [1]

  // für alle x:
  let revOne: Property =
    Prop.forAll Arb.int (fun x ->
      rev [x] = [x])

  let revBug list =
    match list with
    | 1::list' -> rev list'
    | _ -> rev list

  let revrev: Property =
    Prop.forAll (Arb.list Arb.int) (fun list ->
      revBug (revBug list) = list)


// Menge von ints als Liste von Intervallen
type ISet = list<int * int>

// Invariante: Es gibt nur 1 ISet pro Menge von Zahlen
// 1. Die Intervalle überlappen nicht
// 2. Die Intervalle stehen nicht direkt nebeneinander
// 3. Bei jedem Paar (lo, hi) gilt lo <= hi


module ISet =
    // Interval in Liste von Zahlen umwandeln
    let rec range lo hi =
      if lo > hi
      then []
      else lo :: (range (lo + 1) hi)

    // zwei sortierte Listen verschmelzen
    let rec merge2 list1 list2 =
        match (list1, list2) with
        | ([], list2) -> list2
        | (list1, []) -> list1
        | (first1::rest1, first2::rest2) ->
            if first1 = first2
            then first1 :: (merge2 rest1 rest2)
            else if first1 < first2
            then first1 :: (merge2 rest1 list2)
            else first2 :: (merge2 list1 rest2)

    let mmerge (llist: list<list<int>>): list<int> =
        List.fold merge2 [] llist

    let toList iset =
        mmerge (List.map (fun (lo, hi) -> range lo hi) iset)

     let union (iset1: ISet) (iset2: ISet): ISet =
        ???