package body Generics is

   procedure Swap
     (x, y: in out T)
   is
      z : T := x;
   begin
      x := y;
      y := z;
   end Swap;

   function Compose
     (x: A) return C
   is
   begin
      return G(F(x));
   end Compose;

   package body Tuples is

      function Map
        (s : ARRAY_TYPE;
         f : access function (x: ELEMENT_TYPE) return ELEMENT_TYPE)
        return ARRAY_TYPE
      is
      begin
         pragma Assert(s'Length > 0);

         return t : ARRAY_TYPE(s'Range) do
            for k in s'Range loop
               t(k) := f(s(k));
            end loop;
         end return;
      end map;

      function Member
        (x : ELEMENT_TYPE;
         s : ARRAY_TYPE) return BOOLEAN
      is
      begin 
         return (for some k in s'Range => x = s(k));
      end Member;

      function Reversed
        (s : ARRAY_TYPE) return ARRAY_TYPE
      is
         k : INTEGER := INDEX_TYPE'Pos(INDEX_TYPE'First);
      begin
         return t : ARRAY_TYPE(s'Range) do
            for x of reverse s loop
               t(INDEX_TYPE'Val(k)) := x;
               k := k+1;
            end loop;
         end return;
      end Reversed;

      function Rotated
        (n : INDEX_TYPE;
         s : ARRAY_TYPE) return ARRAY_TYPE
      is
         i : constant INDEX_TYPE :=
               INDEX_TYPE'Val(INDEX_TYPE'Pos(s'Last)  - INDEX_TYPE'Pos(n));
         j : constant INDEX_TYPE :=
               INDEX_TYPE'Val(INDEX_TYPE'Pos(s'First) + INDEX_TYPE'Pos(n));
         k : constant INDEX_TYPE :=
               INDEX_TYPE'Val(INDEX_TYPE'Pos(s'Last)  - INDEX_TYPE'Pos(n) + 1);
         l : constant INDEX_TYPE :=
               INDEX_TYPE'Val(INDEX_TYPE'Pos(s'First) + INDEX_TYPE'Pos(n) - 1);
      begin
         return t : ARRAY_TYPE(s'Range) do
            t(s'First .. i)      := s(j       .. s'Last);
            t(k       .. s'Last) := s(s'First .. l);
         end return;
      end Rotated;

      function Sorted
        (s : ARRAY_TYPE) return ARRAY_TYPE
      is
      begin
         return t : ARRAY_TYPE(s'Range) := s do
            Sort(t);
         end return;
      end Sorted;

      function add 
        (i: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
      is (INDEX_TYPE'Val(INDEX_TYPE'Pos(i) + n))
        with Inline;

      function Unique
        (s : ARRAY_TYPE) return BOOLEAN
      is
      begin
         return s'Length < 2 or else
            (for all k in s'First .. add(s'Last, -1) =>
               (for all h in add(k, 1) .. s'Last => s(h) /= s(k)));
      end Unique;

      function Ordered
        (s : ARRAY_TYPE) return BOOLEAN
      is
      begin
         return s'Length < 2 or else
            (for all k in s'First .. add(s'Last, -1) => s(k) < s(add(k, 1)));
      end Ordered;

   end Tuples;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
