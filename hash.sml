structure Hash : sig
  type hash = Word64.word
  val seed = #[]

  val hash_int = Word64.fromInt
  val hash_bool = Word64.fromInt
  val hash_string = FNVHash.hashString