-- generic
--    with package Instance is new Signature (<>);
--    use Instance;
--    with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
--
package body Generics . Tuples . Arrayed is

   procedure Reverse_It
     (t : in out ARRAY_TYPE)
   is
      procedure swap is new Swapper (ELEMENT_TYPE);

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
      procedure swap is new Swapper (ELEMENT_TYPE);
   begin
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
      procedure sub (t: in out ARRAY_TYPE) with Inline
      is begin Left_Rotate_It(n, t); end;

      function fn is new Tuples.Functional (Instance, sub);
   begin
      return fn(t);
   end Left_Rotated;

   procedure Right_Rotate_It
     (n : in     NATURAL;
      t : in out ARRAY_TYPE)
   is
   begin
      Left_Rotate_It(t'Length - n, t);
   end Right_Rotate_It;

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
      function fn is new Squasher (Instance, Member);
   begin
      return fn(t);
   end Remove_Duplicates;

end Generics . Tuples . Arrayed;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
