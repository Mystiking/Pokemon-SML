
fun readFile file =
    let val instrm = TextIO.openIn file
    in
      (String.fields (fn c => c = #"\n")
                                 (TextIO.inputAll(instrm)))
end;

fun makeSkabelonAble (xs) = map (fn (x) => "|" ^ x ^ "|") xs


fun skabelon (xs) = String.concat (map (fn (x) => x ^ "\n") xs)


fun print s = (TextIO.output(TextIO.stdOut, s);
               TextIO.flushOut TextIO.stdOut)

fun Lines 0 acc = acc
  | Lines n acc = Lines (n - 1) ("-" ^ acc);

fun addLines [] = ""
  | addLines (x::xs) = (Lines (size x) "\n") ^
                       ((skabelon(makeSkabelonAble(x::xs)))) ^
                       (Lines (size x) "\n")

fun makePokemonPicture file = skabelon(makeSkabelonAble(readFile (file)))

fun makeRealPokemonPicture file =
    addLines (readFile file)

fun changeIntoScreenPic file =
    let
      val outstrm = TextIO.openOut ("new"^file)
    in (TextIO.output (outstrm, (makeRealPokemonPicture file));
        TextIO.flushOut outstrm)
end

;load "Random";
val rng = Random.newgen ()

val attacksFlying = {Scratch=5, Gust=10, Whirlwind=15}

fun randomAttackFlying {Scratch=5, Gust=10, Whirlwind=15} =
    if Random.random rng < 0.4 then #Scratch attacksFlying
    else if Random.random rng < 0.8 andalso Random.random rng > 0.4
    then #Gust attacksFlying
    else #Whirlwind attacksFlying


fun delOp str = String.fields (fn c => c = #"\n") str

fun readFile file =
    let
      val instrm = TextIO.openIn file
    in
      String.fields (fn c => c = #"\n") (TextIO.inputAll instrm)
      before TextIO.closeIn instrm
    end

fun makeRecord (x1::x2::x3::x4::x5::x5::xs) =
    {name=x1, health=x2, xp_given=x3, 
