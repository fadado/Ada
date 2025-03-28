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
   -- Swap
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

   ---------------------------------------------------------------------
   -- Compose
   ---------------------------------------------------------------------
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

   ------------------------------------------------------------------
   -- Tuples.Location
   ------------------------------------------------------------------

   declare
      use Text_Location;

      Not_Found : exception renames Generics.Tuples.Not_Found;

      s : STRING := "mi mama me mima";
      t : STRING := s;
   begin
      pragma Assert(Reversed("aeiou") = "uoiea");
      pragma Assert(Rotated(1, "aeiou") = "eioua");
      pragma Assert(Rotated(5-1, "aeiou") = "uaeio");

      Reverse_It(s);
      pragma Assert(s = "amim em amam im");

      Reverse_It(t(1..8));
      Reverse_It(t(9..15));
      pragma Assert(t = " amam imamim em");

      s := "mi mama me mima";
      Rotate_It(3, s);
      pragma Assert(s = "mama me mimami ");

      s := "mi mama me mima";
      Rotate_It(15-3, s);
      pragma Assert(s = "imami mama me m");
   end;

   ------------------------------------------------------------------
   -- Tuples.Uniquity
   ------------------------------------------------------------------

   declare
      use Text_Uniquity;

      Not_Found : exception renames Generics.Tuples.Not_Found;

      s : STRING := "mi mama me mima";
      t : STRING := s;
   begin
      pragma Assert(Is_Unique("aeiou"));
      pragma Assert(not Is_Unique("aeioua"));

      pragma Assert(Member('i', "aeiou"));
      pragma Assert(not Member('x', "aeiou"));


      pragma Assert(Position('i', "aeiou") = 3);
      begin
         raised := FALSE;
         pragma Assert(Position('x', "aeiou") = 99);
      exception
         when Not_Found => raised := TRUE;
      end;
      pragma Assert(raised);
   end;

end Tests_Generics;
-- �ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
