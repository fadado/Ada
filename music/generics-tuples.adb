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
         s : ARRAY_TYPE (t'Range);
         i : INDEX_TYPE := s'First;
      begin
         for j in t'Range loop
            if j = t'Last or else
               not Member(t(j), t(INDEX_TYPE'Succ(j) .. t'Last)) 
            then
               s(i) := t(j);
               exit when i = s'Last;
               i := INDEX_TYPE'Succ(i);
            end if;
         end loop;

         if i = s'Last then -- no duplicates found
            return s;
         else
            return s(s'First .. INDEX_TYPE'Pred(i));
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
         procedure swap is
            new Generics.Swap (ELEMENT_TYPE);

         i : INDEX_TYPE := t'First;
         j : INDEX_TYPE := t'Last;
      begin
         while i < j loop
            swap(t(i), t(j));
            i := INDEX_TYPE'Succ(i);
            j := INDEX_TYPE'Pred(j);
         end loop;
      end Reverse_It;

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
         procedure R (t: in out ARRAY_TYPE) with Inline is
         begin
            Rotate_It(n, t);
         end;
         function F is new Functor (Source, R);
      begin
         return F(t);
      end Rotated;

    --generic
    --   with package Target is new Signature (<>);
    --   with function Map (X: in ELEMENT_TYPE) return Target.ELEMENT_TYPE;
      function Mapper
        (t : in ARRAY_TYPE) return Target.ARRAY_TYPE
      is
         subtype TI is Target.INDEX_TYPE;
         subtype TA is Target.ARRAY_TYPE;

         first : constant TI := TI'First;
         last  : constant TI := TI'Val(TI'Pos(first) + t'Length - 1);

         use type TI; -- makes compiler happy!
      begin
         return result : TA (first .. last) do
            pragma Assert(t'Length = result'Length);

            declare
               i : TI := first;
            begin
               for e of t loop
                  result(i) := Map(e);
                  exit when i = result'Last;
                  i := TI'Succ(i);
               end loop;
            end;
         end return;
      end Mapper;

    --generic
    --   with package Target is new Signature (<>);
    --   with function Zip (X, Y: in ELEMENT_TYPE) return Target.ELEMENT_TYPE;
      function Zipper
        (s, t : in ARRAY_TYPE) return Target.ARRAY_TYPE
      is
         subtype TI is Target.INDEX_TYPE;
         subtype TA is Target.ARRAY_TYPE;

         first : constant TI := TI'First;
         last  : constant TI := TI'Val(TI'Pos(first) + t'Length - 1);
      begin
         -- require: s'Length = t'Length and then s'First = t'First

         return result : TA (first .. last) do
            pragma Assert(t'Length = result'Length);

            declare
               i : Source.INDEX_TYPE := s'first;
            begin
               for j in result'Range loop
                  result(j) := Zip(s(i), t(i));
                  exit when i = s'Last;
                  i := Source.INDEX_TYPE'Succ(i);
               end loop;
            end;
         end return;
      end Zipper;

    --generic
    --   with function Test (X: in ELEMENT_TYPE) return BOOLEAN;
      function Filter
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      is
         s : ARRAY_TYPE (t'Range);
         i : INDEX_TYPE := s'First;
      begin
         for e of t loop
            if Test(e) then
               s(i) := e;
               exit when i = s'Last;
               i := INDEX_TYPE'Succ(i);
            end if;
         end loop;

         if i = s'Last then -- all e accepted
            return s;
         else
            return s(s'First .. INDEX_TYPE'Pred(i));
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
   package body Equiv is
   ---------------------------------------------------------------------
 
      function Member
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return BOOLEAN
      is
      begin
         return (for some i in t'Range => x = t(i));
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
 
      function Is_Set
        (t : in ARRAY_TYPE) return BOOLEAN
      is
      begin
         return t'Length < 2 or else
            (for all i in t'First .. INDEX_TYPE'Pred(t'Last) =>
               (for all j in INDEX_TYPE'Succ(i) .. t'Last =>
                  not (t(j) = t(i))));
      end Is_Set;
 
      function As_Set
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      is
         function squash is new squasher (Source, Member);
      begin
         return squash(t);

         -- ensure: Is_Set(As_Set'Result);
      end As_Set;

   end Equiv;
 
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

      function Is_Set
        (t : in ARRAY_TYPE) return BOOLEAN
      is
      begin
         -- require: Is_Sorted(t)

         return t'Length < 2 or else
            (for all i in t'First .. INDEX_TYPE'Pred(t'Last)
               => t(i) < t(INDEX_TYPE'Succ(i)));
      end Is_Set;
 
      function As_Set
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      is
         function squash is new squasher (Source, Member);
      begin
         -- require: Is_Sorted(t);

         return squash(t);

         -- ensure: Is_Set(As_Set'Result);
      end As_Set;

   end Order;

end Generics.Tuples;
-- �ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
