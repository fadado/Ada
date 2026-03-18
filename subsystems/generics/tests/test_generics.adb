pragma Assertion_Policy(Check); -- Check / Ignore

with Generics.Tuples;
with Generics.Tuples.Arrayed;
with Generics.Tuples.Ordered;

use Generics;

procedure Test_Generics is

   package Text_Signature is
      new Tuples.Tuple_Signature (CHARACTER, POSITIVE,  STRING);

   package Text_Arrayed is
      new Tuples.Arrayed (Text_Signature, "=");

   raised : BOOLEAN;

begin
   ------------------------------------------------------------------
   -- Generics.Tuples.Arrayed
   ------------------------------------------------------------------

   declare
      use Text_Arrayed;

      s : STRING := "mi mama me mima";
      t : STRING := s;
   begin
      pragma Assert (Reversed("aeiou") = "uoiea");
      pragma Assert (Left_Rotated(1, "aeiou") = "eioua");
      pragma Assert (Left_Rotated(5-1, "aeiou") = "uaeio");

      Reverse_It(s);
      pragma Assert (s = "amim em amam im");

      Reverse_It(t(1..8));
      Reverse_It(t(9..15));
      pragma Assert (t = " amam imamim em");

      s := "mi mama me mima";
      Left_Rotate_It(3, s);
      pragma Assert (s = "mama me mimami ");

      s := "mi mama me mima";
      Left_Rotate_It(15-3, s);
      pragma Assert (s = "imami mama me m");

      pragma Assert(not Contains_Duplicates("aeiou"));
      pragma Assert (Contains_Duplicates("aeioua"));

      pragma Assert(Member('i', "aeiou"));
      pragma Assert (not Member('x', "aeiou"));


      pragma Assert (Search('i', "aeiou") = 3);
      begin
         raised := FALSE;
         pragma Assert (Search('x', "aeiou") = 99);
      exception
         when Not_Found => raised := TRUE;
      end;
      pragma Assert (raised);
   end;

end Test_Generics;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
