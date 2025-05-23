pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;
with Music;
with Music.MIDI;
with Music.Names;
with Music.DataBase;

procedure Tests_Music is
   use Ada.Text_IO;
   use Music;
   package DB renames Music.DataBase;
begin
   ---------------------------------------------------------------------
   -- MIDI pitch encoding
   ---------------------------------------------------------------------
   declare
      use MIDI;
      x, y : PITCH;
      i, j : PITCH_INTERVAL;
      u : UNORDERED_INTERVAL;
   begin
      x := 60; y := 67;
      i := Interval(x, y);
      j := Interval(y, x);
      u := abs Interval(y, x);

      pragma Assert(i = 7);
      pragma Assert(j = -7);
      pragma Assert(u = 7);

      pragma Assert(Transposition(i, x) = y);
      pragma Assert(Transposition(j, y) = x);
   end;

   ---------------------------------------------------------------------
   -- Pitch-class and intervals
   ---------------------------------------------------------------------
   declare
      x, y : PITCH_CLASS;
      i, j : PC_INTERVAL;
      u : INTERVAL_CLASS;
   begin
      x := 0; y := 7;
      i := Interval(x, y);
      j := Interval(y, x);
      u := abs j;

      pragma Assert(i = 7);
      pragma Assert(j = 5);
      pragma Assert(u = 5);

      pragma Assert(Transposition(i, x) = y);
      pragma Assert(Transposition(j, y) = x);
      pragma Assert(x + i = y);
      pragma Assert(y + j = x);

      x := 0; i := 0; pragma Assert(Inversion(i, x) = 0);
      x := 0; i := 5; pragma Assert(Inversion(i, x) = 5);
      x := 1; i := 5; pragma Assert(Inversion(i, x) = 4);
      x := 5; i := 2; pragma Assert(Inversion(i, x) = 9);
      x := 2; i := 5; pragma Assert(x - i = 9);

      i := -PC_INTERVAL'(1);  pragma Assert(i = 11);
      i := -PC_INTERVAL'(11); pragma Assert(i = 1);
   end;

   begin
      for x in PITCH_CLASS loop
         for y in PITCH_CLASS loop
            pragma Assert(Interval(x,y) = -Interval(y,x));

            for z in PITCH_CLASS loop
               pragma Assert(Interval(x,y)+Interval(y,z) = Interval(x,z));
            end loop;
         end loop;
      end loop;

      for i in PC_INTERVAL loop
         pragma Assert(i + 0  = i);
         pragma Assert(i + (-i) = 0);

         for j in PC_INTERVAL loop
            for k in PC_INTERVAL loop
               pragma Assert((i+j)+k = i+(j+k));
            end loop;
         end loop;

         for x in PITCH_CLASS loop
            pragma Assert(Interval(x,Transposition(i,x)) = i);
            pragma Assert(Inversion(i,x) = Transposition(i,-x));
            pragma Assert(Inversion(i,x) = i-x);
         end loop;
      end loop;
   end;

   declare
      use Music.Names;

      s : HEPTACHORD := (0,2,4,5,7,9,11);
      t : PENTACHORD := (1,3,6,8,10);
      diatonic  : PC_SET := DB.Name_To_Set(DB.Diatonic);
      chromatic : PC_SET := DB.Name_To_Set(DB.Chromatic);
   begin
      pragma Assert(Set(s) = diatonic);
      for x of s loop
         pragma Assert(Member(x, chromatic));
         pragma Assert(Member(x, diatonic));
      end loop;
      for x of t loop
         pragma Assert(Member(x, chromatic));
         pragma Assert(not Member(x, diatonic));
      end loop;
   end;

   declare
      function identity(x: PITCH_CLASS) return PITCH_CLASS
        is (Transposition(0, x)) with Inline;
   begin
      for x in PITCH_CLASS loop
         for i in PC_INTERVAL loop
            pragma Assert(identity(Transposition(i,x)) = Transposition(i,x));
            pragma Assert(Transposition(i,identity(x)) = Transposition(i,x));
            pragma Assert(identity(Inversion(i,x)) = Inversion(i,x));
            pragma Assert(Inversion(i,identity(x)) = Inversion(i,x));
         end loop;
      end loop;
   end;

   declare
      type PAIR is record a, b : PITCH_CLASS; end record;
      pairs : array (POSITIVE range <>) of PAIR
         := ((1,11),(2,10),(3,9),(4,8),(5,7),(6,6));
      i, j : PC_INTERVAL;
      c, d : INTERVAL_CLASS;
   begin
      for p of pairs loop
         i := Interval(p.a,p.b);
         j := Interval(p.b,p.a);
         c := abs i;
         d := abs j;
         pragma Assert(-i = j);
         pragma Assert(-j = i);
         pragma Assert(NATURAL(p.a)+NATURAL(p.b) = 12);
         pragma Assert(c = d);
         pragma Assert(INTERVAL_CLASS'(abs i) = abs j);
      end loop;
   end;
  
   declare
      use Music.Database;
      use Music.Names;

      names : array (POSITIVE range <>) of SCALE_NAME := (
         Lydian,
         Major,
         Ionian,
         Mixolydian,
         Dorian,
         Minor,
         Aeolian,
         Phrygian,
         Locrian
      );
      subtype SCALE is
         TUPLE_INDEX range TUPLE_INDEX'First..TUPLE_INDEX'First+6;
      modes : array (POSITIVE range <>) of INTERVAL_PATTERN(SCALE) :=
      (  (2,2,2,1,2,2,1),
         (2,2,1,2,2,2,1),
         (2,2,1,2,2,2,1),
         (2,2,1,2,2,1,2),
         (2,1,2,2,2,1,2),
         (2,1,2,2,1,2,2),
         (2,1,2,2,1,2,2),
         (1,2,2,2,1,2,2),
         (1,2,2,1,2,2,2)
      );
      s : HEPTACHORD;
   begin
      for k in names'Range loop
         s := Tuple(Name_To_Set(names(k)));
         pragma Assert(Pattern(s) = modes(k));
      end loop;
   end;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   declare
      use Music.Names;

      diatonic   : constant PC_SET := DB.Name_To_Set(DB.Diatonic);
      pentatonic : constant PC_SET := DB.Name_To_Set(DB.Pentatonic);
      whole_tone : constant PC_SET := DB.Name_To_Set(DB.Whole_Tone);

      x, y : TRICHORD;
      raised : BOOLEAN;

   begin
      pragma Assert(HEPTACHORD'(0,2,4,5,7,9,11) = Tuple(diatonic));
      pragma Assert(diatonic = Set(HEPTACHORD'(0,2,4,5,7,9,11)));
      pragma Assert(Transposition(2, diatonic) = 2#011010110101#);

      pragma Assert(diatonic = Set(Tuple(diatonic)));

      x := TRICHORD'(0,1,4);
      y := TRICHORD'(0,11,8);
      pragma Assert(Inversion(x) = y);
      x := TRICHORD'(0,4,7);
      y := TRICHORD'(0,8,5);
      pragma Assert(Inversion(x) = y);

      pragma Assert(diatonic   = Generate(F, 7, Perfect_5));
      pragma Assert(diatonic   = Generate(B, 7, Perfect_4));
      pragma Assert(pentatonic = Generate(C, 5, Perfect_5));
      pragma Assert(whole_tone = Generate(C, 6, Major_2));

      pragma Assert(Retrograde(Tuple(diatonic)) = HEPTACHORD'(11,9,7,5,4,2,0));
      pragma Assert(Rotate(1, Tuple(diatonic))  = HEPTACHORD'(11,0,2,4,5,7,9));
      pragma Assert(Rotate(2, Tuple(diatonic))  = HEPTACHORD'(9,11,0,2,4,5,7));

      y := TRICHORD'(7,3,1);
      pragma Assert(y /= TRICHORD'(1,3,7));
      pragma Assert(Sorted(y) /= y);
      x := Sorted(y);
      pragma Assert(Sorted(y) = TRICHORD'(1,3,7));
      Sort(y);
      pragma Assert(y = TRICHORD'(1,3,7));
      pragma Assert(x = y);
      pragma Assert(Search(PITCH_CLASS'(1), y) = 1);
      pragma Assert(Search(PITCH_CLASS'(3), y) = 2);
      pragma Assert(Search(PITCH_CLASS'(7), y) = 3);

      begin
         raised := FALSE;
         pragma Assert(Search(PITCH_CLASS'(2), y) = 3);
      exception
         when Not_Found => raised := TRUE;
      end;
      pragma Assert(raised);
   end;

end Tests_Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
