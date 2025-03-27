pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;
with Generics;

procedure Tests_Generics is
   use Ada.Text_IO;
begin
   ---------------------------------------------------------------------
   -- Generics
   ---------------------------------------------------------------------

   declare
      package G renames Generics;

      procedure Swap is new G.Swap(INTEGER);
      procedure Swap is new G.Swap(STRING);

      a, b : INTEGER;
      v : array (1..2) of INTEGER := (1, 2);
      s : STRING := "Helo";
      t : STRING := "Hola";
   begin
      a := 1; b := 2;
      Swap(a, b);
      pragma Assert(a = 2 and b = 1);
      Swap(v(1), v(2));
      pragma Assert(v = (2,1));
      Swap(s, t);
      pragma Assert(s = "Hola");
   end;

   declare
      package G renames Generics;

      function as_int(x: FLOAT) return INTEGER is (INTEGER(x));
      function as_str(x: INTEGER) return STRING is (x'Image);

      function float_to_string is new G.Compose (
         FLOAT, INTEGER, STRING,
            as_int,  as_str
      );
   begin
      pragma Assert(float_to_string(3.14) = " 3");
   end;

end Tests_Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
