pragma Assertion_Policy(Check);

with Generics.Tuples;

procedure Silent_Tuples
is
begin
   ---------------------------------------------------------------------
   -- Generics.Tuples.Tuple_Signature
   -- Generics.Tuples.Functional
   ---------------------------------------------------------------------
   declare
      package Tuple_String is
         new Generics.Tuples.Tuple_Signature (
            CHARACTER, POSITIVE, STRING
      );

      procedure P(s: in out STRING)
      is
      begin
         for i in s'Range loop
            if s(i) = ' ' then
               s(i) := '#';
            end if;
         end loop;
      end;

      function F is
         new Generics.Tuples.Functional (
            Tuple_String, P
      );

      S : STRING := "mi mama me mima mucho";
      T : STRING := "mi#mama#me#mima#mucho";
   begin
      pragma Assert (F(S) = T);
   end;

   ---------------------------------------------------------------------
   -- Empty tuples
   ---------------------------------------------------------------------
   declare
      type INDEX is range 1..7;
      type EMPTY_VECTOR is
         array (INDEX range 1..INDEX'Pred(INDEX'First)) of INTEGER;
      type COPY_TYPE is array (EMPTY_VECTOR'Range) of INTEGER;
   begin
      pragma Assert (EMPTY_VECTOR'First  = 1);
      pragma Assert (EMPTY_VECTOR'Last   = 0);
      pragma Assert (EMPTY_VECTOR'Length = 0);

      pragma Assert (COPY_TYPE'First  = 1);
      pragma Assert (COPY_TYPE'Last   = 0);
      pragma Assert (COPY_TYPE'Length = 0);

      pragma Assert (INDEX'Pred(INDEX'First) = 0);
   end;

end Silent_Tuples;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
