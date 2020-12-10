namespace Contract

module C =
  // Zero-Coupon Bond / Zero-Bond
  // Ich bekomme am 31.12.2020 1000€.

  // Swap:
  // Am 31.12.2020:
  // Ich bekomme 1000€.
  // Ich bezahle 1000GBP.

  // Swap:
  // Ich bekomme am 31.12.2020 1000€ UND
  // Ich zahle am 31.12.2020 1000GBP
  type Date = string

  type Amount = double

  type Currency = EUR | GBP

  (*
  type Contract =
    | Zcb of Date * Amount * Currency

  let c1 = Zcb ("2020-12-31", 100.0, EUR)
  *)

  type Direction = Long | Short

  type Contract =
    | Zero
    | One of Currency
    | Multiple of Amount * Contract // Kombinator
    | Later of Date * Contract
    | Give of Contract
    | And of Contract * Contract  // IMMER SUCHEN!

  let c1 = Later ("2020-12-31", Multiple (100.0, One EUR))

  let zcb date amount currency = Later (date, Multiple (amount, One currency))

  // smart constructor
  let multiple amount contract =
    match contract with
    | Zero -> Zero
    | _ -> Multiple (amount, contract)

  let c1' = zcb "2020-12-31" 100.0 EUR

  // Ich bezahle 100GBP am 31.12.2020
  let c2 = Give (zcb "2020-12-31" 100.0 GBP)

  let swap = And (c1, c2)

  let c3 = Multiple (100.0, Multiple (50.0, One EUR))

  let c4 = Multiple (100.0, And (One EUR, Later ("2020-12-31", One EUR)))

  let c5 = And (Multiple (100.0, One EUR), Later ("2020-12-31", One EUR))

  type Payment = Payment of Date * Direction * Amount * Currency

  let scalePayment factor (Payment (date, direction, amount, currency)) =
    Payment (date, direction, amount * factor, currency)

  // Zurück kommt ein "Restvertrag"
  let rec step (contract: Contract) (now: Date): list<Payment> * Contract =
    match contract with
    | Zero -> ([], Zero)
    | One currency -> ([Payment (now, Long, 1.0, currency)], Zero)
    | Multiple (amount, contract') ->
        let (payments, residualContract') = step contract' now
        (List.map (scalePayment amount) payments,
         multiple amount residualContract')
    | Later (date, contract') ->
        if now < date
        then ([], contract)
        else step contract' now
    // | Give contract' -> ...
    | And (contract1, contract2) ->
      let (payments1, residualContract1) = step contract1 now
      let (payments2, residualContract2) = step contract2 now
      (List.append payments1 payments2,
       And (residualContract1, residualContract2))

