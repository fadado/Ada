-- Generates permutations

pragma Assertion_Policy(Ignore); -- Check / Ignore

with Ada.Text_IO;

with Generics.Depth_First_Search;

procedure Permute is

   type ITEM   is range 1..5;
   type INDEX  is range 1..5;
   type VECTOR is array (INDEX) of ITEM;

   Used_Items : array (ITEM) of BOOLEAN := (others => FALSE);

   procedure Goal(v: VECTOR)
   is
      use Ada.Text_IO;
      voyels : STRING := "AEIOU";
   begin
      for t of v loop
         Put(' ');
         Put(voyels(INTEGER(t)));
      end loop;
      New_Line;
   end Goal;

   function Rejected(v: VECTOR; i: INDEX; e: ITEM) return BOOLEAN
   with Inline,
        Pre => i > INDEX'First
   is
   begin
      return Used_Items(e);
   end;

   procedure Enter(v: VECTOR; i: INDEX; e: ITEM)
   with Inline
   is
   begin
      Used_Items(e) := TRUE;
   end;

   procedure Leave(v: VECTOR; i: INDEX; e: ITEM)
   with Inline
   is
   begin
      Used_Items(e) := FALSE;
   end;

begin
   declare
      package Permutations is
         new Generics.Depth_First_Search (
           ARRAY_TYPE   => VECTOR,
           INDEX_TYPE   => INDEX,
           ELEMENT_TYPE => ITEM
         );
   begin
      Permutations.Seek;
   end;
end Permute;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
