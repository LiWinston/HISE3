with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   type TokenArray is array(Positive range <>) of TokenExtent;

   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   -- The Tokenise procedure splits a string into whitespace-separated tokens,
   -- storing each token´s position (start and length) into the output Tokens array.
   --
   -- Postconditions:
   -- - Count (number of tokens found) never exceeds the array size of Tokens.
   -- - Each token written to the array has:
   --   1. A starting index within the bounds of the input string,
   --   2. A positive, non-zero length,
   --   3. A range that doesn't run past the end of the input string.
   --
   -- These postconditions ensure we never write outside the array bounds,
   -- and that each token stored is meaningful and valid.

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
     Post => Count <= Tokens'Length and
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>
          (Tokens(Index).Start >= S'First and
          Tokens(Index).Length > 0) and then
            Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);


end MyStringTokeniser;
