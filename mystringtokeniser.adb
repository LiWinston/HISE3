package body MyStringTokeniser with SPARK_Mode is



   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) is
      Index : Positive;
      Extent : TokenExtent;
      Processed : Natural := 0;
      OutIndex : Integer := Tokens'First;
   begin
      if (S'First > S'Last) then
         Count := Processed;
         return;
      end if;
      Index := S'First;
      while OutIndex <= Tokens'Last and Index <= S'Last and Processed < Tokens'Length loop

         -- This loop invariant ensures that for all tokens we've already written,
         -- their start positions are within the bounds of the input string,
         -- they are not empty (Length > 0),
         -- and their full range (Start + Length - 1) stays within the input.
         pragma Loop_Invariant
           (for all J in Tokens'First..OutIndex-1 =>
              (Tokens(J).Start >= S'First and
                   Tokens(J).Length > 0) and then
            Tokens(J).Length-1 <= S'Last - Tokens(J).Start);

         -- This invariant maintains the critical relationship between OutIndex and Processed,
         -- which is essential for formal verification for several reasons:
         -- 1. It establishes a precise correspondence between the next available position 
         --    in Tokens array (OutIndex) and the number of tokens processed (Processed).
         -- 2. It enables the SPARK prover to verify that we never write beyond the bounds
         --    of the Tokens array by ensuring OutIndex increases synchronously with Processed.
         -- 3. It helps verify that we never skip positions or overwrite existing tokens.
         -- 4. Without this invariant, the SPARK prover cannot establish that the array 
         --    access at Tokens(OutIndex) is safe and within bounds.
         -- 5. It contributes to proving the postcondition that exactly Count tokens 
         --    have been properly written to the array.
         pragma Loop_Invariant (OutIndex = Tokens'First + Processed);

         -- look for start of next token
         while (Index >= S'First and Index < S'Last) and then Is_Whitespace(S(Index)) loop
            Index := Index + 1;
         end loop;
         if (Index >= S'First and Index <= S'Last) and then not Is_Whitespace(S(Index)) then
            -- found a token
            Extent.Start := Index;
            Extent.Length := 0;

            -- look for end of this token
            while Positive'Last - Extent.Length >= Index
              and then (Index+Extent.Length >= S'First and Index+Extent.Length <= S'Last)
              and then not Is_Whitespace(S(Index+Extent.Length)) loop
               Extent.Length := Extent.Length + 1;
            end loop;

            Tokens(OutIndex) := Extent;
            Processed := Processed + 1;

            -- check for last possible token, avoids overflow when incrementing OutIndex
            if (OutIndex = Tokens'Last) then
               Count := Processed;
               return;
            else
               OutIndex := OutIndex + 1;
            end if;

            -- check for end of string, avoids overflow when incrementing Index
            if S'Last - Extent.Length < Index then
               Count := Processed;
               return;
            else
               Index := Index + Extent.Length;
            end if;
         end if;
      end loop;
      Count := Processed;
   end Tokenise;

end MyStringTokeniser;
