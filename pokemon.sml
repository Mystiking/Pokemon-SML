(* POKEMON EMACSERALD : BETA 1.2 *)

(*  This function reads a "battle" (a picture made from ASCII stuff) *)
fun readBattle battle =
    let
      val pokemonBattle = TextIO.openIn battle
    in
      TextIO.inputAll(pokemonBattle) before TextIO.closeIn pokemonBattle
    end

(*  This is the text used to guide the player throughout the game *)
val death = "Your Pokemon died! Thanks for playing!\nBetter luck next time!\n"
val velkomst =  "Welcome to POKEMON IN SML!\nAre you willing to take on the dangers of the DIKU-region?\n"
val vilDuSpille = "\nDo you wanna continue an old save, or start a new adventure?\n"
val farvel = "\nGoodbye for now\n"
val encounter_0 = "\nIt's about time for your first pokemon battle!\nAre you feeling ready for the challenge?\n"
val encounter_0_pika = "An enemy Pidgey has appeared!\n"
val encounter_0_squirtle = "An enemy Pidgey has appeared!\n"
val pokemonBattle = readBattle "pokemonBattle_1_0.txt";
val oak = "\nProffessor Oak would like to know if you are a boy or a girl\n(Dosen't he always?). So what is it? Are you a boy or a girl?\n"
val noGrills = "The pokemon world is too harsh for girls sry :-)\n"
val battle_stance_1 = "\nFight    Run\nQuit    Save\n"
val battle_stance_pika = "ThunderBolt     Tackle\nScratch         Thunder\n"

;load "Random";
val rng = Random.newgen ()

val attacksFlying = {Scratch=5, Gust=10, Whirlwind=15}

(*  This function allows flying type pokemon to do random attacks *)
fun randomAttackFlying {Scratch=5, Gust=10, Whirlwind=15} =
    if Random.random rng < 0.4 then #Scratch attacksFlying
    else if Random.random rng < 0.8 andalso Random.random rng > 0.4
    then #Gust attacksFlying
    else #Whirlwind attacksFlying;

(*  This function allows me to tell the game to wait x milliseconds
 *  before running the next function. Credits : Sebbe *)
fun sleep n =
    let
      val start = Time.now ()
      fun loop () = if Time.toMilliseconds (Time.- (Time.now (), start)) < n
                    then loop ()
                    else ()
    in loop () end


(*  This function uses the program wv_player.exe to play a sound *)
fun playSound name = Mosml.run "wv_player.exe" ["sounds\\" ^ name] "";

(*  Turns a string into lower chars only *)
fun readable s = String.map Char.toLower s

(*  Does the same as readable but for the Option type input *)
fun lower x = Option.map readable x


local
(*  This function removes newlines *)
fun sletListe [] = raise Empty
  | sletListe [x] = x
  | sletListe (x::xs) = x
in
fun removeNewLine s = sletListe(String.fields (fn (c) => c = #"\n") s)
end

(*  This function prints a string into the stdOut-stream *)
fun print s = (TextIO.output(TextIO.stdOut, s);
               TextIO.flushOut TextIO.stdOut)

(*  The pokemons (so far!) have their status stored in these records *)
val pika = {name="Pikachu", health=50, xp_given=235, xp=0, lvl=1}
val pidgey = {name="Pidgey", health=30, xp_given=50, xp=0, lvl=1}
val squirtle = {name="Squirtle", health=55, xp_given=250, xp=0, lvl=1}

(*  This functions prints text during a battle so the trainer can see the effect
 *  of his choices *)
fun battle {name=str, health=int1, xp_given=int2, xp=int3, lvl=int4} n =
    "Health " ^ str ^ " : " ^ Int.toString (int1 - n) ^ "\n"

(*  This one kinda explains itself :^) *)
fun pokeBattleWon PlayerP () =
    print "You've won your first battle!";

(*  This functions controls the battle between pikachu and another pokemon *)
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
        | NONE => print farvel))

(*  This function calculates how much damage is inflicted upon the pokemons *)
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


(*  This function is the one that will let a player choose between figthing
 *  or running *)
fun firstEncounterPika_0 () =
    ( playSound "pokemonBattle.mp3";
      sleep 1000;
      playSound "016Cry.mp3";
      TextIO.output(TextIO.stdOut, (readBattle "newpokemonBattle_1_0.txt"));
      print encounter_0_pika; print battle_stance_1;
      (case lower(TextIO.inputLine TextIO.stdIn) of
           SOME "fight\n" => (playSound "025Cry.mp3";
                              battleInstancePika_1 pika pidgey ())
         | SOME "run\n" => print "under construction :S \n"
         | SOME _ => (print "Write fight or run!\n";
                      firstEncounterPika_0 ())
         | NONE => print farvel ));

(*  This function asks the player if he is ready to prove is worth in his
 *  first encounter *)
fun firstEncounterPika () =
( sleep 500;
  print encounter_0;
 (case lower(TextIO.inputLine TextIO.stdIn) of
      SOME "yes\n" => (firstEncounterPika_0 ())
    | SOME "no\n" => print farvel
    | SOME _ => (print "Please wrtie yes or no\n"; firstEncounterPika ())
    | NONE => print farvel));

(*  The squirtle part isnt finished yet sadly :'( *)
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

(*  Here the player gets to choose his or her very own pokemon! *)
fun chooseYourPokemon () =
    (TextIO.output(TextIO.stdOut, (readBattle "newpokeballs.txt")^"\n");
     TextIO.flushOut TextIO.stdOut;
     print "Pikachu or Squirtle?\n";
     (case lower(TextIO.inputLine TextIO.stdIn) of
         SOME "pikachu\n" =>
 (TextIO.output(TextIO.stdOut, readBattle "newpokemonBattle_0_pika.txt");
  playSound "025Cry.mp3";
  firstEncounterPika ())
       | SOME "squirtle\n" =>
 (TextIO.output(TextIO.stdOut, readBattle "newpokemonBattle_0_squirtle.txt" ^ "\n");
  playSound "007Cry.mp3";
  firstEncounterSquirtle ())
       | SOME _ => (print ("Write pikachu or squirtle!\n");
                    chooseYourPokemon ())
       | NONE => print farvel ));

fun profOak () =
    (print (oak);
     (case lower(TextIO.inputLine TextIO.stdIn) of
          SOME "boy\n" => chooseYourPokemon ()
        | SOME "girl\n" => chooseYourPokemon ()
        | SOME _ => (print "You have to write boy or girl\n")
        | NONE => print farvel;
     profOak()));

(*  This part doesnt work yet *)
fun openOldGame () =
    (print ("Skriv navnet på det gemte spil du vil åbne\n");
     (case TextIO.inputLine TextIO.stdIn of
          SOME str => let val oldGame = TextIO.openIn (removeNewLine str)
                      in TextIO.output (TextIO.stdOut, TextIO.inputAll oldGame)
                         before TextIO.closeIn oldGame
                      end
        | NONE => print "Farvel!\n");
     TextIO.flushOut TextIO.stdOut)

(*  Lets the player (think) he selects a new game, where data will be stored *)
fun openNewGame () =
    ((print ("Write the name of your save\n");
     (case TextIO.inputLine TextIO.stdIn of
          SOME str =>
          TextIO.output(TextIO.stdOut, readBattle "pokemonBattle_0_1.txt")
        | NONE => print farvel);
     TextIO.flushOut TextIO.stdOut); profOak ());

(*  Starts the game... *)
fun startSpillet () =
    (print ("Do you wanna start a new game, or continue an old one\n");
     (case lower(TextIO.inputLine TextIO.stdIn) of
          SOME "new game\n" => openNewGame ()
        | SOME "saved game\n" => openOldGame ()
        | SOME _ => (print ("You have to write new game or saved game\n");
                     startSpillet ())
        | NONE => print farvel));


fun medPaaEventyr () =
((case lower(TextIO.inputLine TextIO.stdIn) of
     SOME "yes\n" => startSpillet ()
   | SOME "no\n" => print farvel
   | SOME _ => (print ("Yes or no?\n"); medPaaEventyr ())
   | NONE => (print farvel));
              TextIO.flushOut TextIO.stdOut);

(*  This is the function that starts the magic... *)
fun spilPokemon () =
    (print velkomst; medPaaEventyr())
