package body Generics . Tuples is

 --generic
 --   with package Instance is new Signature (<>);
 --   use Instance;
 --   with procedure Do_It(t: in out ARRAY_TYPE);
   function Functional
   (t : in ARRAY_TYPE) return ARRAY_TYPE
   is
   begin
      return result : ARRAY_TYPE := t do
         Do_It(result);
      end return;
   end Functional;

 --generic
 --   with package Instance is new Signature (<>);
 --   use Instance;
 --   with function Member(x: ELEMENT_TYPE; t: in ARRAY_TYPE) return BOOLEAN;
   function Squasher
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
      squash:
         for j in t'Range loop
            if j = t'Last or else
               not Member(t(j), t(INDEX_TYPE'Succ(j) .. t'Last)) 
            then
               result(i) := t(j);
               exit squash when i = result'Last;
               i := INDEX_TYPE'Succ(i);
            end if;
         end loop squash;

         if i = result'Last then -- no duplicates found
            return result;
         else
            return result(result'First .. INDEX_TYPE'Pred(i));
         end if;
      end;
   end Squasher;

end Generics . Tuples;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
