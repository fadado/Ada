------------------------------------------------------------------------
package body Generics.Tuples is
------------------------------------------------------------------------

 --generic
 --   with package Signature_Package is new Signature (<>);
 --   use Signature_Package;
 --   with procedure Do_It(t: in out ARRAY_TYPE);
   function Functional
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   is
   begin
      return result : ARRAY_TYPE(t'Range) := t do
         Do_It(result);
      end return;
   end Functional;

 --generic
 --   with package Signature_Package is new Signature (<>);
 --   use Signature_Package;
 --   with function Map (x: in ELEMENT_TYPE) return ELEMENT_TYPE;
   procedure Procedural
     (t : in out ARRAY_TYPE)
   is
   begin
      for i in t'Range loop
         t(i) := Map(t(i));
      end loop;
   end Procedural;

   ---------------------------------------------------------------------
 --generic
 --   with package Signature_Package is new Signature (<>);
 --   use Signature_Package;
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
         function F is new Functional (Signature_Package, R);
      begin
         return F(t);
      end Rotated;

      --------------
      -- Functors --
      --------------

    --generic
    --   with function Map (x: in ELEMENT_TYPE) return ELEMENT_TYPE;
      function Applier
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      is
         procedure P is new Procedural (Signature_Package, Map);
         function  F is new Functional (Signature_Package, P);
      begin
         return F(t);
      end Applier;
 
    --generic
    --   with package Output is new Signature (<>);
    --   with function Map (x: in ELEMENT_TYPE) return Output.ELEMENT_TYPE;
      function Mapper
        (t : in ARRAY_TYPE) return Output.ARRAY_TYPE
      is
         subtype OI is Output.INDEX_TYPE;
         subtype OA is Output.ARRAY_TYPE;

         first : constant INTEGER := OI'Pos(OI'First);
         last  : constant INTEGER := first + t'Length - 1;

         use Output; -- makes compiler happy!
      begin
         return result : OA (OI'Val(first) .. OI'Val(last)) do
            pragma Assert(t'Length = result'Length);
            declare
               i : OI := OI'Val(first);
            begin
               for j in t'Range loop
                  result(i) := Map(t(j));
                  exit when i = OI'Last;
                  i := OI'Succ(i);
               end loop;
            end;
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
 --generic
 --   with package Signature_Package is new Signature (<>);
 --   use Signature_Package;
 --   with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
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
 
   end Equiv;
 
   ---------------------------------------------------------------------
 --generic
 --   with package Signature_Package is new Signature (<>);
 --   use Signature_Package;
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
 
      function Member
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return BOOLEAN
      is
      begin
         return Search(x, t) >= t'First; -- always true if found
      exception
         when Not_Found => return FALSE;
      end Member;

   end Order;

end Generics.Tuples;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
