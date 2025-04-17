------------------------------------------------------------------------
package body Generics.Tuples is
------------------------------------------------------------------------

   generic
      with package Source is new Signature (<>);
      use Source;
      with function Member(x: ELEMENT_TYPE; t: in ARRAY_TYPE) return BOOLEAN;
   function squasher
     (t : in ARRAY_TYPE) return ARRAY_TYPE;

   function squasher
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   is
   begin
      if t'Length < 2 then
         return t;
      end if;

      declare
         result : ARRAY_TYPE (t'Range);
         i : INDEX_TYPE := result'First;
      begin
         for j in t'Range loop
            if j = t'Last or else
               not Member(t(j), t(INDEX_TYPE'Succ(j) .. t'Last)) 
            then
               result(i) := t(j);
               exit when i = result'Last;
               i := INDEX_TYPE'Succ(i);
            end if;
         end loop;

         if i = result'Last then -- no duplicates found
            return result;
         else
            return result(result'First .. INDEX_TYPE'Pred(i));
         end if;
      end;
   end squasher;

 --generic
 --   with package Source is new Signature (<>);
 --   use Source;
 --   with procedure Do_It(t: in out ARRAY_TYPE);
   function Functor
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   is
   begin
      return result : ARRAY_TYPE := t do
         Do_It(result);
      end return;
   end Functor;

   ---------------------------------------------------------------------
 --generic
 --   with package Source is new Signature (<>);
 --   use Source;
   package body Place is
   ---------------------------------------------------------------------

      procedure Reverse_It
        (t : in out ARRAY_TYPE)
      is
         procedure swap is new Generics.Swap (ELEMENT_TYPE);

         i : INDEX_TYPE := t'First;
         j : INDEX_TYPE := t'Last;
      begin
         while i < j loop
            swap(t(i), t(j));
            i := INDEX_TYPE'Succ(i);
            j := INDEX_TYPE'Pred(j);
         end loop;
      end Reverse_It;

      procedure Left_Rotate_It
        (n : in     NATURAL;
         t : in out ARRAY_TYPE)
      is
         procedure swap is new Generics.Swap (ELEMENT_TYPE);
      begin
         -- require: n <= t'Length

         if n = 0 or else n = t'Length or else t'Length = 1 then
            return;
         end if;

         if t'Length = 2 then
            if n mod 2 /= 0 then -- odd?
               swap(t(t'First), t(t'Last));
            end if;
            return;
         end if;

         declare
            m  : constant INTEGER := INDEX_TYPE'Pos(INDEX_TYPE'First);
            r1 : constant INDEX_TYPE := INDEX_TYPE'Val(m + n - 1);
            r2 : constant INDEX_TYPE := INDEX_TYPE'Succ(r1);
         begin
            Reverse_It(t(t'First .. r1));
            Reverse_It(t(r2      .. t'Last));
            Reverse_It(t);
         end;
      end Left_Rotate_It;

      function Left_Rotated
        (n : in NATURAL;
         t : in ARRAY_TYPE) return ARRAY_TYPE
      is  
         procedure R (t: in out ARRAY_TYPE) with Inline is
         begin
            Left_Rotate_It(n, t);
         end;
         function F is new Functor (Source, R);
      begin
         return F(t);
      end Left_Rotated;

      procedure Right_Rotate_It
        (n : in     NATURAL;
         t : in out ARRAY_TYPE)
      is
      begin
         Left_Rotate_It(t'Length - n, t);
      end Right_Rotate_It;

    --generic
    --   with package Target is new Signature (<>);
    --   with function Mapping (X: in ELEMENT_TYPE) return Target.ELEMENT_TYPE;
      function Mapper
        (t : in ARRAY_TYPE) return Target.ARRAY_TYPE
      is
         subtype SI is Source.INDEX_TYPE;
         subtype TI is Target.INDEX_TYPE;
         subtype TA is Target.ARRAY_TYPE;

         first : constant TI := TI'First;
         last  : constant TI := TI'Val(TI'Pos(first) + t'Length - 1);
      begin
         return result : TA (first .. last) do
            pragma Assert(t'Length = result'Length);

            declare
               i : SI := t'First;
            begin
               for e of result loop
                  e := Mapping(t(i));
                  exit when i = t'Last;
                  i := SI'Succ(i);
               end loop;
            end;
         end return;
      end Mapper;

    --generic
    --   with package Target is new Signature (<>);
    --   with function Zipping (X, Y: in ELEMENT_TYPE) return Target.ELEMENT_TYPE;
      function Zipper
        (s, t : in ARRAY_TYPE) return Target.ARRAY_TYPE
      is
         subtype SI is Source.INDEX_TYPE;
         subtype TI is Target.INDEX_TYPE;
         subtype TA is Target.ARRAY_TYPE;

         first : constant TI := TI'First;
         last  : constant TI := TI'Val(TI'Pos(first) + t'Length - 1);
      begin
         -- require: s'Length = t'Length and then s'First = t'First

         return result : TA (first .. last) do
            pragma Assert(t'Length = result'Length);

            declare
               i : SI := t'first;
            begin
               for e of result loop
                  e := Zipping(s(i), t(i));
                  exit when i = t'Last;
                  i := SI'Succ(i);
               end loop;
            end;
         end return;
      end Zipper;

    --generic
    --   with function Test (X: in ELEMENT_TYPE) return BOOLEAN;
      function Filter
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      is
         result : ARRAY_TYPE (t'Range);
         i : INDEX_TYPE := result'First;
      begin
         for e of t loop
            if Test(e) then
               result(i) := e;
               exit when i = result'Last;
               i := INDEX_TYPE'Succ(i);
            end if;
         end loop;

         if i = result'Last then -- all e accepted
            return result;
         else
            return result(result'First .. INDEX_TYPE'Pred(i));
         end if;
      end Filter;

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

         return result : ELEMENT_TYPE := t(t'First) do
            for e of t(INDEX_TYPE'Succ(t'First) .. t'Last) loop
               result := Operation(result, e);
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

         return result : ELEMENT_TYPE := t(t'First) do
            for e of t(INDEX_TYPE'Succ(t'First) .. t'Last) loop
               if Better(e, result) then
                  result := e;
               end if;
            end loop;
         end return;
      end Chooser;

   end Place;

   ---------------------------------------------------------------------
 --generic
 --   with package Source is new Signature (<>);
 --   use Source;
 --   with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   package body Equivalence is
   ---------------------------------------------------------------------
 
      function Member
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return BOOLEAN
      is
      begin
         return (for some e of t => x = e);
      end Member;

      function Search
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return INDEX_TYPE
      is
         -- Linear Search
      begin 
         -- require: t'Length > 0

         for i in t'Range loop
            if x = t(i) then
               return i;
            end if;
         end loop;

         raise Not_Found;
      end Search;
 
      function Contains_Duplicates
        (t : in ARRAY_TYPE) return BOOLEAN
      is
      begin
         return t'Length > 1 and then
            (for some i in t'First .. INDEX_TYPE'Pred(t'Last) =>
               (for some j in INDEX_TYPE'Succ(i) .. t'Last =>
                  t(j) = t(i)));
      end Contains_Duplicates;
 
      function Remove_Duplicates
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      is
         function squash is new squasher (Source, Member);
      begin
         return squash(t);

         -- ensure: not Contains_Duplicates(Remove_Duplicates'Result);
      end Remove_Duplicates;

   end Equivalence;
 
   ---------------------------------------------------------------------
 --generic
 --   with package Source is new Signature (<>);
 --   use Source;
 --   with function "<" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
 --   with function ">" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
 --   with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
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
 
      procedure Sort_It
        (t : in out ARRAY_TYPE)
      is
         function add (m: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
         is (INDEX_TYPE'Val(INDEX_TYPE'Pos(m) + n)) with Inline;
 
         function sub (m: INDEX_TYPE; n: INTEGER) return INDEX_TYPE
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
         function squash is new squasher (Source, Member);
      begin
         -- require: Is_Sorted(t);

         return squash(t);

         -- ensure: not Contains_Duplicates(Remove_Duplicates'Result);
      end Remove_Duplicates;

   end Order;

end Generics.Tuples;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
