package body Music.Tuples is

   function Sorted
     (s : TUPLE_TYPE) return TUPLE_TYPE
   is
   begin
      return t : TUPLE_TYPE(s'Range) := s do
         Sort(t);
      end return;
   end Sorted;

   procedure Swap
     (s : in out TUPLE_TYPE;
      i : in INDEX_TYPE;
      j : in INDEX_TYPE)
   is
      a : ELEMENT_TYPE;
   begin
      a := s(i);
      s(i) := s(j);
      s(j) := a;
   end Swap;

   function Sum
     (s : TUPLE_TYPE) return INTEGER
   is
      a : INTEGER := 0;
   begin
      for n of s loop
         a := a + INTEGER(n);
      end loop;
      return a;
   end Sum;

end Music.Tuples;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
