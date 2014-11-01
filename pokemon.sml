(* POKEMON EMACSERALD : ALPHA 1.0 *)

fun readBattle battle =
    let
      val pokemonBattle = TextIO.openIn battle
    in
      TextIO.inputAll(pokemonBattle) before TextIO.closeIn pokemonBattle
    end
val death = "Your Pokemon died! Thanks for playing!\nBetter luck next time!\n"
val velkomst =  "Velkommen til POKEMON SML : ALPHA 1.0!\nDa dette spil stadig er i Alpha, vil der (forhåbentligt ikke) forekomme en del\nfejl undervejs!\nEr du villig til at drage ind i den forunderlige DIKU-region, trods disse farer?\n"
val vilDuSpille = "\nVil du starte et nyt spil eller vil du fortsætte et gemt eventyr?\n"
val farvel = "\nFarvel for denne gang!\n"
val encounter_0 = "\nDet er ved at være tid til din første battle!\nEr du klar?\n"
val encounter_0_pika = "An enemy Haunter has appeared!\n"
val encounter_0_squirtle = "An enemy Haunter has appeared!\n"
val pokemonBattle = readBattle "pokemonBattle_1_0.txt";
val oak = "\nProfesseor Oak vil gerne vide om du er en dreng eller pige.\nSå snart du har svaret ham, kan du vælge din pokemon.\nDu kan vælge mellem Squirtle og Pikachu. Så er du en dreng eller en grill?\n"
val noGrills = "Dette er ikke en verden der er lavet til en pige desværre! Ses hvis du kan få en af dine drenge venner til at spille\n"
val battle_stance_1 = "\nFight    Run\nQuit    Save\n"
val battle_stance_pika = "ThunderBolt     Tackle\nScratch     Thunder\n"

;load "Random";
val rng = Random.newgen ()

val attacksFlying = {Scratch=5, Gust=10, Whirlwind=15}

fun randomAttackFlying {Scratch=5, Gust=10, Whirlwind=15} =
    if Random.random rng < 0.4 then #Scratch attacksFlying
    else if Random.random rng < 0.8 andalso Random.random rng > 0.4
    then #Gust attacksFlying
    else #Whirlwind attacksFlying
;

fun sleep n =
    let
      val start = Time.now ()
      fun loop () = if Time.toMilliseconds (Time.- (Time.now (), start)) < n
                    then loop ()
                    else ()
    in loop () end


fun playSound name = Mosml.run "wv_player.exe" ["sounds\\" ^ name] "";

fun readable s = String.map Char.toLower s

fun lower x = Option.map readable x


local
fun sletListe [] = raise Empty
  | sletListe [x] = x
  | sletListe (x::xs) = x
in
fun removeNewLine s = sletListe(String.fields (fn (c) => c = #"\n") s)
end

fun print s = (TextIO.output(TextIO.stdOut, s);
               TextIO.flushOut TextIO.stdOut)

val pika = {name="Pikachu", health=50, xp_given=235, xp=0, lvl=1}
val pidgey = {name="Pidgey", health=30, xp_given=50, xp=0, lvl=1}
val squirtle = {name="Squirtle", health=55, xp_given=250, xp=0, lvl=1}

fun battle {name=str, health=int1, xp_given=int2, xp=int3, lvl=int4} n =
    "Health " ^ str ^ " : " ^ Int.toString (int1 - n) ^ "\n"

fun pokeBattleWon PlayerP () =
    print "Du vandt"




fun battleInstancePika_1 {name=str1,health=int1,xp_given=int2,xp=int3,lvl=int4}
                         {name=str2,health=int5,xp_given=int6,xp=int7,lvl=int8}
                         () =
    (print (battle {name=str1, health=int1, lvl=int4,xp_given=int2,xp=int3} 0);
     print (battle {name=str2,health=int5,xp_given=int6,xp=int7,lvl=int8} 0);
     print battle_stance_pika;
     (case lower(TextIO.inputLine TextIO.stdIn) of
          SOME "thunderbolt\n" =>
          damage_1 {name=str1,health=int1,xp_given=int2,xp=int3,lvl=int4}
                   {name=str2,health=int5,xp_given=int6,xp=int7,lvl=int8} 10
        | SOME "tackle\n" => damage_1 pika pidgey 5
        | SOME "scratch\n" => damage_1 pika pidgey 5
        | SOME "thunder\n" => damage_1 pika pidgey 15
        | SOME _ => (print "Write the name of the ability you wish to use\n";
                     battleInstancePika_1 pika pidgey ())
        | NONE => print "Farvel\n"))

and damage_1 {name=str1,health=int1,xp_given=int2,xp=int3,lvl=int4}
             {name=str2,health=int5,xp_given=int6,xp=int7,lvl=int8} x =
    let val EnemyP = {name=str2,health=int5 - x,
                      xp_given=int6,xp=int7,lvl=int8}
        val PlayerP = {name=str1,
                       health=int1, xp_given=int2,xp=int3,lvl=int4}

    in if int5 - x < 1
       then pokeBattleWon PlayerP ()
       else let val PlayerP = {name=str1, health=int1 - randomAttackFlying
                                                            attacksFlying,
                               xp_given=int2,xp=int3,lvl=int4}
            in
              if int1 > 0 then battleInstancePika_1 PlayerP EnemyP ()
              else print death
            end
    end

fun firstEncounterPika_0 () =
    ( playSound "pokemonBattle.mp3";
      sleep 1000;
      playSound "016Cry.mp3";
      TextIO.output(TextIO.stdOut, (readBattle "newpokemonBattle_1_0.txt"));
      print encounter_0_pika; print battle_stance_1;
      (case lower(TextIO.inputLine TextIO.stdIn) of
           SOME "fight\n" => (playSound "025Cry.mp3";
                              battleInstancePika_1 pika pidgey ())
         | SOME "run\n" => print "under oprettelse"
         | SOME _ => (print "Du skal skrive Fight eller Run";
                      firstEncounterPika_0 ())
         | NONE => print "Farvel\n" ));


fun firstEncounterPika () =
( sleep 500;
  print encounter_0;
 (case lower(TextIO.inputLine TextIO.stdIn) of
      SOME "ja\n" => (firstEncounterPika_0 ())
    | SOME "nej\n" => print "Farvel for idag så!\n"
    | SOME _ => (print "Skriv ja eller nej"; firstEncounterPika ())
    | NONE => print "Farvel!\n"));

fun firstEncounterSquirtle_0 () =
    (TextIO.output(TextIO.stdOut, (readBattle "newpokemonBattle_1_0.txt"));
     print encounter_0_squirtle; print battle_stance_1;
     (case lower(TextIO.inputLine TextIO.stdIn) of
          SOME "fight\n" => print "under oprettelse"
        | SOME "run\n" => print "under oprettelse"
        | SOME _ => (print "Du skal skrive Fight eller Run";
                     firstEncounterSquirtle_0 ())
        | NONE => print "Farvel\n"));

fun firstEncounterSquirtle () =
(print encounter_0;
 sleep 1500;
 (case lower(TextIO.inputLine TextIO.stdIn) of
      SOME "ja\n" => ( playSound "093Cry.mp3"; firstEncounterSquirtle_0 ())
    | SOME "nej\n" => print "Farvel for idag så!\n"
    | SOME _ => (print "Skriv ja eller nej"; firstEncounterSquirtle ())
    | NONE => print "Farvel!\n"));

fun chooseYourPokemon () =
    (TextIO.output(TextIO.stdOut, (readBattle "newpokeballs.txt")^"\n");
     TextIO.flushOut TextIO.stdOut;
     print "Pikachu eller Squirtle?\n";
     (case lower(TextIO.inputLine TextIO.stdIn) of
         SOME "pikachu\n" =>
 (TextIO.output(TextIO.stdOut, readBattle "newpokemonBattle_0_pika.txt");
  playSound "025Cry.mp3";
  firstEncounterPika ())
       | SOME "squirtle\n" =>
 (TextIO.output(TextIO.stdOut, readBattle "newpokemonBattle_0_squirtle.txt" ^ "\n");
  playSound "007Cry.mp3";
  firstEncounterSquirtle ())
       | SOME _ => (print ("Skriv enten Pikachu (til højre) eller Squirtle (til venstre)\n");
                    chooseYourPokemon ())
       | NONE => print "Farvel\n" ));

fun profOak () =
    (print (oak);
     (case lower(TextIO.inputLine TextIO.stdIn) of
          SOME "dreng\n" => chooseYourPokemon ()
        | SOME "grill\n" => print noGrills
        | SOME _ => (print "Du skal svare enten dreng eller grill\n")
        | NONE => print "Farvel\n";
     profOak()));

fun openOldGame () =
    (print ("Skriv navnet på det gemte spil du vil åbne\n");
     (case TextIO.inputLine TextIO.stdIn of
          SOME str => let val oldGame = TextIO.openIn (removeNewLine str)
                      in TextIO.output (TextIO.stdOut, TextIO.inputAll oldGame)
                         before TextIO.closeIn oldGame
                      end
        | NONE => print "Farvel!\n");
     TextIO.flushOut TextIO.stdOut)

fun openNewGame () =
    ((print ("Skriv navnet du vil gemme under\n");
     (case TextIO.inputLine TextIO.stdIn of
          SOME str =>
          TextIO.output(TextIO.stdOut, readBattle "pokemonBattle_0_1.txt")
        | NONE => print "Farvel!\n");
     TextIO.flushOut TextIO.stdOut); profOak ());

fun startSpillet () =
    (print ("Vil du starte et nyt spil eller fortsætte et eventyr?\n");
     (case lower(TextIO.inputLine TextIO.stdIn) of
          SOME "nyt spil\n" => openNewGame ()
        | SOME "gemt spil\n" => openOldGame ()
        | SOME _ => (print ("Du skal svare nyt spil eller gemt spil\n");
                     startSpillet ())
        | NONE => print "Farvel!\n"));


fun medPaaEventyr () =
((case lower(TextIO.inputLine TextIO.stdIn) of
     SOME "ja\n" => startSpillet ()
   | SOME "nej\n" => print farvel
   | SOME _ => (print ("Du skal svare ja eller nej\n"); medPaaEventyr ())
   | NONE => (print ("Farvel så!\n")));
              TextIO.flushOut TextIO.stdOut);

fun spilPokemon () =
    (print velkomst; medPaaEventyr())
