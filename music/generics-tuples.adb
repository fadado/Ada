------------------------------------------------------------------------
package body Generics.Tuples is
------------------------------------------------------------------------

   ---------------------------------------------------------------------
   package body Place is
   ---------------------------------------------------------------------

      procedure Reverse_It
        (t : in out ARRAY_TYPE)
      is
         procedure swap is
            new Generics.Swap (ELEMENT_TYPE);

         i : INTEGER := INDEX_TYPE'Pos(t'First);
         j : INTEGER := INDEX_TYPE'Pos(t'Last);
      begin
         while i < j loop
            swap(t(INDEX_TYPE'Val(i)), t(INDEX_TYPE'Val(j)));
            i := i + 1;
            j := j - 1;
         end loop;
      end Reverse_It;

      function Reversed
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      is
      begin
         return result : ARRAY_TYPE(t'Range) := t do
            Reverse_It(result);
         end return;
      end Reversed;

      procedure Rotate_It
        (n : in     INDEX_TYPE;
         t : in out ARRAY_TYPE)
      is
      begin
         Reverse_It(t(t'First .. n));
         Reverse_It(t(INDEX_TYPE'Succ(n) .. t'Last));
         Reverse_It(t);
      end Rotate_It;

      function Rotated
        (n : in INDEX_TYPE;
         t : in ARRAY_TYPE) return ARRAY_TYPE
      is
      begin
         return result : ARRAY_TYPE(t'Range) := t do
            Rotate_It(n, result);
         end return;
      end Rotated;
 
      --------------
      -- Functors --
      --------------

    --generic
    --   with function Map (x: in ELEMENT_TYPE) return ELEMENT_TYPE;
      procedure Apply_To
        (t : in out ARRAY_TYPE)
      is
      begin
         for i in t'Range loop
            t(i) := Map(t(i));
         end loop;
      end Apply_To;

    --generic
    --   with function Map (x: in ELEMENT_TYPE) return ELEMENT_TYPE;
      function Mapper
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      is
      begin
         return result : ARRAY_TYPE(t'Range) do
            for i in t'Range loop
               result(i) := Map(t(i));
            end loop;
         end return;
      end Mapper;
 
    --generic
    --   with function Operation (L, R: in ELEMENT_TYPE) return ELEMENT_TYPE;
      function Reducer
         (t : in ARRAY_TYPE) return ELEMENT_TYPE
      is
      begin 
         -- require: t'Length > 0
         if t'Length = 1 then
            return t(t'First);
         end if;

         -- check: t'Length > 1
         return result : ELEMENT_TYPE := t(t'First) do
            for i in INDEX_TYPE'Succ(t'First) .. t'Last loop
               result := Operation(result, t(i));
            end loop;
         end return;
      end Reducer;
 
    --generic
    --   with function Better (L, R: in ELEMENT_TYPE) return BOOLEAN;
      function Chooser
         (t : in ARRAY_TYPE) return ELEMENT_TYPE
      is
      begin 
         -- require: t'Length > 0
         if t'Length = 1 then
            return t(t'First);
         end if;

         -- check: t'Length > 1
         return result : ELEMENT_TYPE := t(t'First) do
            for i in INDEX_TYPE'Succ(t'First) .. t'Last loop
               if Better(t(i), result) then
                  result := t(i);
               end if;
            end loop;
         end return;
      end Chooser;
   end Place;

   ---------------------------------------------------------------------
   package body Equiv is
   ---------------------------------------------------------------------
 
      function Is_Unique
        (t : in ARRAY_TYPE) return BOOLEAN
      is
      begin
         return t'Length < 2 or else
            (for all i in t'First .. INDEX_TYPE'Pred(t'Last) =>
               (for all j in INDEX_TYPE'Succ(i) .. t'Last =>
                  not (t(j) = t(i))));
      end Is_Unique;
 
      function Position
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return INDEX_TYPE
      is
      begin 
         -- require: t'Length > 0
         for i in t'Range loop
            if x = t(i) then
               return i;
            end if;
         end loop;

         raise Not_Found;
      end Position;
 
   end Equiv;
 
   ---------------------------------------------------------------------
   package body Order is
   ---------------------------------------------------------------------
 
      function Is_Sorted
        (t : in ARRAY_TYPE) return BOOLEAN
      is
      begin
         return t'Length < 2 or else
            (for all i in t'First .. INDEX_TYPE'Pred(t'Last)
               => t(i) < t(INDEX_TYPE'Succ(i))
                  or else not(t(i) > t(INDEX_TYPE'Succ(i))));
      end Is_Sorted;
 
      function Is_Unique
        (t : in ARRAY_TYPE) return BOOLEAN
      is
      begin
         -- require: Is_Sorted(t)
         return t'Length < 2 or else
            (for all i in t'First .. INDEX_TYPE'Pred(t'Last)
               => t(i) < t(INDEX_TYPE'Succ(i)));
      end Is_Unique;
 
      procedure Sort_It
        (t : in out ARRAY_TYPE)
      is
         function add(m: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
         is (INDEX_TYPE'Val(INDEX_TYPE'Pos(m) + n)) with Inline;
 
         function sub(m: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
         is (INDEX_TYPE'Val(INDEX_TYPE'Pos(m) - n)) with Inline;
 
         -- Shell Sort
         increment : NATURAL := t'Length / 2;
         j, k      : INDEX_TYPE;
         tmp       : ELEMENT_TYPE;
      begin
         while increment > 0 loop
            for i in add(t'First, increment) .. t'Last loop
               tmp := t(i);
               j   := i;
               k   := sub(j, increment);
               while j >= add(t'First, increment) and then
                     tmp < t(k) loop
                  k    := sub(j, increment);
                  t(j) := t(k);
                  j    := k;
               end loop;
               t(j) := tmp;
            end loop;
            increment := increment / 2;
         end loop;
         -- ensure: Is_Sorted(t)
      end Sort_It;
 
      function Sorted
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      is
      begin
         return result : ARRAY_TYPE(t'Range) := t do
            Sort_It(result);
         end return;
      end Sorted;
 
      function Search
        (t : in ARRAY_TYPE; x : in ELEMENT_TYPE) return INDEX_TYPE
      is
         -- Binary Search
         low    : INDEX_TYPE := t'First;
         high   : INDEX_TYPE := t'Last;
         middle : INDEX_TYPE;
      begin
         -- require: Is_Sorted(t)
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
 
         raise Not_Found;
      end Search;
 
   end Order;

end Generics.Tuples;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
