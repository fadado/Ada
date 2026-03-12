-- Generates combinations

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;

with Generics.Depth_First_Search;

procedure Combine
is
   Bits : constant := 8;

   type TINY is range 0..2**Bits-1;

   procedure Combine_String
     (CSet : in STRING;
      N    : in TINY)
   is
      first : constant TINY := TINY(CSet'First);
      last  : constant TINY := TINY(CSet'Last);

      subtype ITEMS   is TINY range first .. last;
      subtype INDICES is TINY range first .. first+N-1;

      type COMBINATION is array (INDICES) of ITEMS
         with Pack,
              Component_Size => Bits;

      Used_Items : array (ITEMS) of BOOLEAN := (others => FALSE);

      procedure Goal
        (solution : COMBINATION)
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
        (candidate : COMBINATION;
         index     : INDICES;
         item      : ITEMS) return BOOLEAN
      with Inline,
           Pre => index > INDICES'First and then index <= INDICES'Last
      is
      begin
         return Used_Items(item) or else item < index;
      end;

      procedure Enter
        (candidate : COMBINATION;
         index     : INDICES;
         item      : ITEMS)
      with Inline,
           Pre  => not Used_Items(item),
           Post => Used_Items(item) 
      is
      begin
         Used_Items(item) := TRUE;
      end;

      procedure Leave
        (candidate : COMBINATION;
         index     : INDICES;
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
         package Combinations is
            new Generics.Depth_First_Search (
               INDEX_TYPE   => INDICES,
               ELEMENT_TYPE => ITEMS,
               ARRAY_TYPE   => COMBINATION
            );
      begin
         Combinations.Seek;
      end;
   end Combine_String;

begin
   -- main
   Combine_String("ABC", 2);
end Combine;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
