pragma Assertion_Policy(Check);

with Generics;

procedure Silent_Generics
is
begin
   ---------------------------------------------------------------------
   -- Generics.Identity
   ---------------------------------------------------------------------
   declare
      function id is new Generics.Identity (INTEGER);
      function id is new Generics.Identity (STRING);
   begin
      pragma Assert (id(8) = 8);
      pragma Assert (id("STR1") = "STR1");
   end;

   ---------------------------------------------------------------------
   -- Generics.Swapper
   ---------------------------------------------------------------------
   declare
      procedure swap is new Generics.Swapper(INTEGER);
      procedure swap is new Generics.Swapper(STRING);

      a, b : INTEGER;
      v : array (1..2) of INTEGER := (1, 2);

      s : STRING := "Helo";
      t : STRING := "Hola";
   begin
      a := 1; b := 2;
      swap(a, b);
      pragma Assert(a = 2 and b = 1);

      swap(v(1), v(2));
      pragma Assert(v = (2,1));

      swap(s, t);
      pragma Assert(s = "Hola" and t = "Helo");
   end;

   ---------------------------------------------------------------------
   -- Generics.Compose
   ---------------------------------------------------------------------
   declare
      function to_integer(x: FLOAT) return INTEGER is (INTEGER(x));
      function to_string(x: INTEGER) return STRING is (x'Image);

      function float_integer_string is new Generics.Compose (
         FLOAT, INTEGER, to_integer, STRING, to_string
      );
   begin
      pragma Assert (float_integer_string(3.14) = " 3");
   end;

   ---------------------------------------------------------------------
   -- Generics.Partial
   ---------------------------------------------------------------------
   declare
      function sum(a, b: INTEGER) return INTEGER is (a+b);
      function sum(a, b: STRING) return STRING is (a&b);

      function sum7 is new Generics.Partial (
         INTEGER, INTEGER, INTEGER, sum, 7
      );
      function sumTHE is new Generics.Partial (
         STRING, STRING, STRING, sum, "the "
      );
   begin
      pragma Assert (sum7(3) = 10);
      pragma Assert (sumTHE("object") = "the object");
   end;

end Silent_Generics;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
