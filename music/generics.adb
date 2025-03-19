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

   ---------------------------------------------------------------------
   package body Any_Tuples is
   ---------------------------------------------------------------------

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

      function Mapper
        (t : ARRAY_TYPE) return ARRAY_TYPE
      is
      begin
         return r : ARRAY_TYPE(t'Range) do
            for i in t'Range loop
               r(i) := map(t(i));
            end loop;
         end return;
      end Mapper;

      function Reducer
         (t : ARRAY_TYPE) return ELEMENT_TYPE
      is
      begin 
         return r : ELEMENT_TYPE := t(t'First) do
            for i in INDEX_TYPE'Succ(t'First) .. t'Last loop
               r := reduce(r, t(i));
            end loop;
         end return;
      end Reducer;

      function Chooser
         (t : ARRAY_TYPE) return ELEMENT_TYPE
      is
      begin 
         return r : ELEMENT_TYPE := t(t'First) do
            for i in INDEX_TYPE'Succ(t'First) .. t'Last loop
               if better(t(i), r) then
                  r := t(i);
               end if;
            end loop;
         end return;
      end Chooser;

   end Any_Tuples;

   ---------------------------------------------------------------------
   package body Eq_Tuples is
   ---------------------------------------------------------------------

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

      function Is_Unique
        (t : ARRAY_TYPE) return BOOLEAN
      is
      begin
         return t'Length < 2 or else
            (for all i in t'First .. INDEX_TYPE'Pred(t'Last) =>
               (for all j in INDEX_TYPE'Succ(i) .. t'Last =>
                  t(j) /= t(i)));
      end Is_Unique;

   end Eq_Tuples;

   ---------------------------------------------------------------------
   package body Ord_Tuples is
   ---------------------------------------------------------------------

      -- Shell Sort

      procedure Sort
        (t : in out ARRAY_TYPE)
      is
         function add(m: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
         is (INDEX_TYPE'Val(INDEX_TYPE'Pos(m) + n)) with Inline;

         function sub(m: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
         is (INDEX_TYPE'Val(INDEX_TYPE'Pos(m) - n)) with Inline;

         increment : NATURAL := t'Length / 2;
         j, k      : INDEX_TYPE;
         tmp       : ELEMENT_TYPE;
      begin
         while increment > 0 loop
            for i in add(t'First, increment) .. t'Last loop
               tmp := t(i);
               j   := i;
               k   := sub(j, increment);
               while j   >= add(t'First, increment) and then
                     tmp <  t(k) loop
                  k    := sub(j, increment);
                  t(j) := t(k);
                  j    := k;
               end loop;
               t(j) := tmp;
            end loop;
            increment := increment / 2;
         end loop;
         pragma Assert(Is_Sorted(t));
      end Sort;

      function Sorted
        (t : ARRAY_TYPE) return ARRAY_TYPE
      is
      begin
         return r : ARRAY_TYPE(t'Range) := t do
            Sort(r);
         end return;
      end Sorted;

      function Is_Sorted
        (t : ARRAY_TYPE) return BOOLEAN
      is
      begin
         return t'Length < 2 or else
            (for all i in t'First .. INDEX_TYPE'Pred(t'Last)
               => t(i) < t(INDEX_TYPE'Succ(i)));
      end Is_Sorted;

      -- Binary Search

      function Search
        (t : ARRAY_TYPE; x : ELEMENT_TYPE) return INDEX_TYPE
      is
         low    : INDEX_TYPE := t'First;
         high   : INDEX_TYPE := t'Last;
         middle : INDEX_TYPE;
      begin
         pragma Assert(Is_Sorted(t));

         while low <= high loop
            middle := INDEX_TYPE'Val((INDEX_TYPE'Pos(low) +
                                      INDEX_TYPE'Pos(high)) / 2);
            if t(middle) < x then
               low := INDEX_TYPE'Succ(middle);
            elsif t(middle) > x then
               high := INDEX_TYPE'Pred(middle);
            else
               return middle;
            end if;
         end loop;

         raise Constraint_Error;
      end Search;

   end Ord_Tuples;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
