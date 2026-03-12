-- Generates permutations

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;

with Generics.Tuples;

with Sequences;

procedure Permute
is
   use Ada.Text_IO;

   package String_Tuple is
      new Generics.Tuples.Tuple_Signature (CHARACTER, POSITIVE, STRING);

   codes : constant STRING := "ABC";   --"0123456789";

   package P is new Sequences (
      String_Tuple,
      Objects  => codes,
      N        => codes'Length,
      Repeated => FALSE
   );

   permutations : P.ITERABLE_TYPE;

begin

   each_permutation:
      for permutation of permutations loop

      each_index:
         for index of permutation loop
            Put(' ' & codes(POSITIVE(index)));
         end loop each_index;

         New_Line;
      end loop each_permutation;

   -- Output for "RGB":
   --    R G B
   --    R B G
   --    G R B
   --    G B R
   --    B R G
   --    B G R

end Permute;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
