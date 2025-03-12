package body Generics is

   procedure Swap
     (x, y: in out T)
   is
      z : constant T := x;
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

   package body Eq_Tuples is

      function Map
        (t : ARRAY_TYPE;
         f : access function (x: ELEMENT_TYPE) return ELEMENT_TYPE)
        return ARRAY_TYPE
      is
      begin
         pragma Assert(t'Length > 0);

         return r : ARRAY_TYPE(t'Range) do
            for i in t'Range loop
               r(i) := f(t(i));
            end loop;
         end return;
      end map;

      function Member
        (x : ELEMENT_TYPE;
         t : ARRAY_TYPE) return BOOLEAN
      is
      begin 
         return (for some i in t'Range => x = t(i));
      end Member;

      function Position
        (x : ELEMENT_TYPE;
         t : ARRAY_TYPE) return INDEX_TYPE
      is
      begin 
         for i in t'Range loop
            if t(i) = x then
               return i;
            end if;
         end loop;
         raise Constraint_Error;
      end Position;

      function Reversed
        (t : ARRAY_TYPE) return ARRAY_TYPE
      is
         i : INTEGER := INDEX_TYPE'Pos(INDEX_TYPE'First);
      begin
         return r : ARRAY_TYPE(t'Range) do
            for x of reverse t loop
               r(INDEX_TYPE'Val(i)) := x;
               i := i+1;
            end loop;
         end return;
      end Reversed;

      function Rotated
        (n : INDEX_TYPE;
         t : ARRAY_TYPE) return ARRAY_TYPE
      is
         subtype NDX is INDEX_TYPE;
         i : constant NDX := NDX'Val(NDX'Pos(t'Last)  - NDX'Pos(n));
         j : constant NDX := NDX'Val(NDX'Pos(t'First) + NDX'Pos(n));
         k : constant NDX := NDX'Val(NDX'Pos(t'Last)  - NDX'Pos(n) + 1);
         l : constant NDX := NDX'Val(NDX'Pos(t'First) + NDX'Pos(n) - 1);
      begin
         return r : ARRAY_TYPE(t'Range) do
            r(t'First .. i)      := t(j       .. t'Last);
            r(k       .. t'Last) := t(t'First .. l);
         end return;
      end Rotated;

      function Unique
        (t : ARRAY_TYPE) return BOOLEAN
      is
         function add 
           (i: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
         is (INDEX_TYPE'Val(INDEX_TYPE'Pos(i) + n)) with Inline;
      begin
         return t'Length < 2 or else
            (for all i in t'First .. add(t'Last, -1) =>
               (for all j in add(i, 1) .. t'Last => t(j) /= t(i)));
      end Unique;

   end Eq_Tuples;

   package body Ord_Tuples is

      function Sorted
        (t : ARRAY_TYPE) return ARRAY_TYPE
      is
      begin
         return r : ARRAY_TYPE(t'Range) := t do
            Sort(r);
         end return;
      end Sorted;

      function Is_Ordered
        (t : ARRAY_TYPE) return BOOLEAN
      is
         function add 
           (i: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
         is (INDEX_TYPE'Val(INDEX_TYPE'Pos(i) + n)) with Inline;
      begin
         return t'Length < 2 or else
            (for all i in t'First .. add(t'Last, -1) => t(i) < t(add(i, 1)));
      end Is_Ordered;

   end Ord_Tuples;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
