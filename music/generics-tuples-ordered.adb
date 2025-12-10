--generic
--   with package Instance is new Signature (<>);
--   use Instance;
--   with function "<" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
--   with function ">" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
--   with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;

package body Generics . Tuples . Ordered is

   function Is_Sorted
     (t : in ARRAY_TYPE) return BOOLEAN
   is
   begin
      return t'Length < 2 or else
         (for all i in t'First .. INDEX_TYPE'Pred(t'Last)
            => t(i) < t(INDEX_TYPE'Succ(i))
               or else not(t(i) > t(INDEX_TYPE'Succ(i))));
   end Is_Sorted;

   procedure Sort_It
     (t : in out ARRAY_TYPE)
   is
      function plus (m: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
      is (INDEX_TYPE'Val(INDEX_TYPE'Pos(m) + n)) with Inline;

      function minus (m: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
      is (INDEX_TYPE'Val(INDEX_TYPE'Pos(m) - n)) with Inline;

      -- Shell Sort
      increment : NATURAL := t'Length / 2;
      j, k      : INDEX_TYPE;
      tmp       : ELEMENT_TYPE;
   begin
      while increment > 0 loop
         for i in plus(t'First, increment) .. t'Last loop
            tmp := t(i);
            j   := i;
            k   := minus(j, increment);

            while j >= plus(t'First, increment) and then
                  tmp < t(k) loop
               k    := minus(j, increment);
               t(j) := t(k);
               j    := k;
            end loop;

            t(j) := tmp;
         end loop;

         increment := increment / 2;
      end loop;

      -- ensure: Is_Sorted(t)
   end Sort_It;

   function Member
     (x : in ELEMENT_TYPE;
      t : in ARRAY_TYPE) return BOOLEAN
   is
      -- Linear Search on sorted array
   begin
      -- require: Is_Sorted(t);

      for e of t loop
         if e < x then
            null;
         elsif e > x then
            return FALSE;
         else
            return TRUE;
         end if;
      end loop;

      return FALSE;
   end Member;

   function Search
     (x : in ELEMENT_TYPE;
      t : in ARRAY_TYPE) return INDEX_TYPE
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

   function Contains_Duplicates
     (t : in ARRAY_TYPE) return BOOLEAN
   is
   begin
      -- require: Is_Sorted(t)

      return t'Length > 1 and then
         (for some i in t'First .. INDEX_TYPE'Pred(t'Last)
            => not (t(i) < t(INDEX_TYPE'Succ(i))));
   end Contains_Duplicates;

   function Remove_Duplicates
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   is
      function fn is new Squasher (Instance, Member);
   begin
      -- require: Is_Sorted(t);

      return fn(t);

      -- ensure: not Contains_Duplicates(Remove_Duplicates'Result);
   end Remove_Duplicates;

end Generics . Tuples . Ordered;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
