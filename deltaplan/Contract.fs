namespace Contract

module C =
  // Zero-Coupon Bond / Zero-Bond
  // Ich bekomme am 31.12.2020 1000€.

  // Swap:
  // Am 31.12.2020:
  // Ich bekomme 1000€.
  // Ich bezahle 1000GBP.
  type Date = string

  type Amount = double

  type Currency = EUR | GBP

  (*
  type Contract =
    | Zcb of Date * Amount * Currency

  let c1 = Zcb ("2020-12-31", 100.0, EUR)
  *)

  type Contract =
    | One of Currency
    | Multiple of Amount * Contract // Kombinator
    | Later of Date * Contract

  let c1 = Later ("2020-12-31", Multiple (100.0, One EUR))

  let zcb date amount currency = Later (date, Multiple (amount, One currency))

  let c1' = zcb "2020-12-31" 100.0 EUR
