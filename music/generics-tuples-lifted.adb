--generic
--   with package Instance is new Signature (<>);
--   use Instance;
package body Generics . Tuples . Lifted is

 --generic
 --   with package Target is new Signature (<>);
 --   with function Map (X: in ELEMENT_TYPE) return Target.ELEMENT_TYPE;
   function Mapper
     (t : in ARRAY_TYPE) return Target.ARRAY_TYPE
   is
      subtype SI is Instance.INDEX_TYPE;
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
         apply:
            for e of result loop
               e := Map(t(i));
               exit apply when i = t'Last;
               i := SI'Succ(i);
            end loop apply;
         end;
      end return;
   end Mapper;

 --generic
 --   with package Target is new Signature (<>);
 --   with function Zip (X, Y: in ELEMENT_TYPE) return Target.ELEMENT_TYPE;
   function Zipper
     (s, t : in ARRAY_TYPE) return Target.ARRAY_TYPE
   is
      subtype SI is Instance.INDEX_TYPE;
      subtype TI is Target.INDEX_TYPE;
      subtype TA is Target.ARRAY_TYPE;

      first : constant TI := TI'First;
      last  : constant TI := TI'Val(TI'Pos(first) + t'Length - 1);
   begin
      return result : TA (first .. last) do
         pragma Assert(t'Length = result'Length);

         declare
            i : SI := t'first;
         begin
         apply:
            for e of result loop
               e := Zip(s(i), t(i));
               exit apply when i = t'Last;
               i := SI'Succ(i);
            end loop apply;
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
   apply:
      for e of t loop
         if Test(e) then
            result(i) := e;
            exit apply when i = result'Last;
            i := INDEX_TYPE'Succ(i);
         end if;
      end loop apply;

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

end Generics . Tuples . Lifted;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
