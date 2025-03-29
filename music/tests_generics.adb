pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;
with Generics;

use Ada.Text_IO;

procedure Tests_Generics is

   package Text_Signature is
      new Generics.Tuples.Signature (POSITIVE, CHARACTER, STRING);

   package Text_Functors is
      new Generics.Tuples.Functors (Text_Signature);

   package Text_Location is
      new Generics.Tuples.Location (Text_Signature);

   package Text_Uniquity is
      new Generics.Tuples.Uniquity (Text_Signature, "=");

   package Text_Order is
      new Generics.Tuples.Order (Text_Signature, "<", ">");

   package G renames Generics;

   raised : BOOLEAN;

begin
   ---------------------------------------------------------------------
   -- Generics
   ---------------------------------------------------------------------

   declare
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
      function as_int(x: FLOAT) return INTEGER is (INTEGER(x));
      function as_str(x: INTEGER) return STRING is (x'Image);

      function float_to_string is new G.Compose (
         FLOAT, INTEGER, STRING,
            as_int,  as_str
      );
   begin
      pragma Assert(float_to_string(3.14) = " 3");
   end;

   declare
   begin
      pragma Assert(Text_Location.Reversed("aeiou") = "uoiea");
      pragma Assert(Text_Location.Rotated(1, "aeiou") = "eioua");
      pragma Assert(Text_Location.Rotated(5-1, "aeiou") = "uaeio");

      pragma Assert(Text_Uniquity.Is_Unique("aeiou"));
      pragma Assert(not Text_Uniquity.Is_Unique("aeioua"));
      pragma Assert(Text_Uniquity.Member('i', "aeiou"));
      pragma Assert(not Text_Uniquity.Member('x', "aeiou"));
      pragma Assert(Text_Uniquity.Position('i', "aeiou") = 3);

      begin
         raised := FALSE;
         pragma Assert(Text_Uniquity.Position('x', "aeiou") = 99);
      exception
         when Generics.Tuples.Not_Found => raised := TRUE;
      end;
      pragma Assert(raised);
   end;

end Tests_Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
