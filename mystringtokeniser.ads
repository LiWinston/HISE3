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
   -- storing each token's position (start and length) into the output Tokens array.
   --
   -- Postconditions:
   -- 1. Count <= Tokens'Length: Ensures the returned token count never exceeds 
   --    the provided array size, preventing array bounds violations.
   -- 
   -- 2. For all valid token indices (Tokens'First..Tokens'First+(Count-1)):
   --    a. Tokens(Index).Start >= S'First: Ensures each token's starting position
   --       is within the bounds of the input string, preventing illegal memory access.
   --    b. Tokens(Index).Length > 0: Ensures each token has at least one character
   --       (non-empty), avoiding processing of meaningless tokens.
   --    c. Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start: Ensures each token's
   --       range (Start to Start+Length-1) doesn't exceed the input string bounds,
   --       preventing access beyond the string's end.
   --
   -- These postconditions collectively guarantee memory safety and data integrity
   -- by ensuring all operations remain within valid array and string boundaries.

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
     Post => Count <= Tokens'Length and
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>
          (Tokens(Index).Start >= S'First and
          Tokens(Index).Length > 0) and then
            Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);


end MyStringTokeniser;
