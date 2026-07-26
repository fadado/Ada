pragma Assertion_Policy(Check);

with Generics.Tuples;
with Generics.Tuples.Arrayed;

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

   ------------------------------------------------------------------
   -- Generics.Tuples.Arrayed
   ------------------------------------------------------------------
   declare
      use Generics;

      package Text_Signature is
         new Tuples.Tuple_Signature (CHARACTER, POSITIVE,  STRING);

      package Text_Arrayed is
         new Tuples.Arrayed (Text_Signature, "=");
      use Text_Arrayed;

      raised : BOOLEAN;
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

end Silent_Tuples;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
