pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;
with Generics.Tuples;

use Ada.Text_IO;
use Generics;

procedure Tests_Generics is

   package Text_Signature is
      new Tuples.Signature (POSITIVE, CHARACTER, STRING);

   package Text_Place is
      new Tuples.Place (Text_Signature);

   package Text_Equiv is
      new Tuples.Equiv (Text_Signature, "=");

   package Text_Order is
      new Tuples.Order (Text_Signature, "<", ">");

   raised : BOOLEAN;

begin
   ---------------------------------------------------------------------
   -- Empty arrays and slice
   ---------------------------------------------------------------------
   declare
      type INDEX is range 1..2;
      type EMPTY is array (INDEX range 1..INDEX'Pred(INDEX'First)) of INTEGER;
      type XXXXX is array (EMPTY'Range) of INTEGER;
   begin
      Pragma Assert(EMPTY'First  = 1);
      Pragma Assert(EMPTY'Last   = 0);
      Pragma Assert(EMPTY'Length = 0);
      Pragma Assert(XXXXX'First  = 1);
      Pragma Assert(XXXXX'Last   = 0);
      Pragma Assert(XXXXX'Length = 0);
      Pragma Assert(INDEX'Pred(INDEX'First) = 0);
   end;

   ---------------------------------------------------------------------
   -- Swap
   ---------------------------------------------------------------------

   declare
      procedure swap is new Generics.Swap(INTEGER);
      procedure swap is new Generics.Swap(STRING);

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
      pragma Assert(s = "Hola");
   end;

   ---------------------------------------------------------------------
   -- Compose
   ---------------------------------------------------------------------
   declare
      function as_int(x: FLOAT) return INTEGER is (INTEGER(x));
      function as_str(x: INTEGER) return STRING is (x'Image);

      function float_to_string is new Compose (
         FLOAT, INTEGER, STRING,
            as_int,  as_str
      );
   begin
      pragma Assert(float_to_string(3.14) = " 3");
   end;

   ------------------------------------------------------------------
   -- Tuples.Place
   ------------------------------------------------------------------

   declare
      use Text_Place;

      Not_Found : exception renames Generics.Not_Found;

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
   -- Tuples.Equiv
   ------------------------------------------------------------------

   declare
      use Text_Equiv;

      Not_Found : exception renames Generics.Not_Found;

      s : STRING := "mi mama me mima";
      t : STRING := s;
   begin
      pragma Assert(Is_Unique("aeiou"));
      pragma Assert(not Is_Unique("aeioua"));

      pragma Assert(Member('i', "aeiou"));
      pragma Assert(not Member('x', "aeiou"));


      pragma Assert(Search('i', "aeiou") = 3);
      begin
         raised := FALSE;
         pragma Assert(Search('x', "aeiou") = 99);
      exception
         when Not_Found => raised := TRUE;
      end;
      pragma Assert(raised);
   end;

end Tests_Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
