-- Generates permutations

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;

with Generics.Depth_First_Search;

procedure Permute
is
   Bits : constant := 8;

   type TINY is range 0..2**Bits-1;

   procedure Permute_String(CSet: in STRING)
   is
      subtype ITEMS is TINY range TINY(CSet'First)..TINY(CSet'Last);

      type PERMUTATION is array (ITEMS) of ITEMS
         with Pack, Component_Size => Bits;

      Used_Items : array (ITEMS) of BOOLEAN := (others => FALSE);

      procedure Goal
        (solution : PERMUTATION)
      is
         use Ada.Text_IO;
      begin
         for t of solution loop
            Put(' ');
            Put(CSet(POSITIVE(t)));
         end loop;
         New_Line;
      end Goal;

      function Rejected
        (candidate : PERMUTATION;
         index     : ITEMS;
         item      : ITEMS) return BOOLEAN
      with Inline, Pre => index > ITEMS'First
      is
      begin
         return Used_Items(item);
      end;

      procedure Enter
        (candidate : PERMUTATION;
         index     : ITEMS;
         item      : ITEMS)
      with Inline,
           Pre  => not Used_Items(item),
           Post => Used_Items(item) 
      is
      begin
         Used_Items(item) := TRUE;
      end;

      procedure Leave
        (candidate : PERMUTATION;
         index     : ITEMS;
         item      : ITEMS)
      with Inline,
           Pre  => Used_Items(item),
           Post => not Used_Items(item) 
      is
      begin
         Used_Items(item) := FALSE;
      end;

   begin
      declare
         package Permutations is
            new Generics.Depth_First_Search (
               INDEX_TYPE   => ITEMS,
               ELEMENT_TYPE => ITEMS,
               ARRAY_TYPE   => PERMUTATION
            );
      begin
         Permutations.Seek;
      end;
   end Permute_String;

begin
   -- main
   Permute_String("ABC");
end Permute;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
