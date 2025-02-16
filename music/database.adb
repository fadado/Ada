package body DataBase is

   PCS : constant array (SCALE_NAME) of PC_SET := (
      Acoustic =>            2#101010110110#,
      Aeolian =>             2#101101011010#,
      Augmented_Triad =>     2#100010001000#,
      Chromatic =>           2#111111111111#,
      Diatonic =>            2#101011010101#,
      Diminished_7th =>      2#100100100100#,
      Dorian =>              2#101101010110#,
      Double_Harmonic =>     2#110011011001#,
      Harmonic_Lidyan =>     2#101010110110#,
      Harmonic_Major =>      2#101011011001#,
      Harmonic_Minor =>      2#101101011001#,
      Hungarian_Major_I =>   2#100110110110#,
      Hungarian_Major_II =>  2#100110110101#,
      Ionian =>              2#101011010101#,
      Locrian =>             2#110101101010#,
      Locrian_Major =>       2#101011101010#,
      Lydian =>              2#101010110101#,
      Lydian_Dominant =>     2#101010110110#,
      Lydian_Minor =>        2#101010111010#,
      Major =>               2#101011010101#,
      Melodic =>             2#101101010101#,
      Minor =>               2#101101011010#,
      Mixolydian =>          2#101011010110#,
      Monotonic =>           2#100000000000#,
      Octatonic =>           2#101101101101#,
      Pentatonic =>          2#101010010100#,
      Pentatonic1 =>         2#101010010100#,
      Pentatonic2 =>         2#101001010100#,
      Pentatonic3 =>         2#101001010010#,
      Pentatonic4 =>         2#100101010010#,
      Pentatonic5 =>         2#100101001010#,
      Phrygian =>            2#110101011010#,
      Spanish_Phrygian =>    2#110011011010#,
      Tritone =>             2#100000100000#,
      Whole_Tone =>          2#101010101010# 
   );

   function Name_To_Set
     (name : SCALE_NAME) return PC_SET
   is
   begin
      return PCS(name);
   end Name_To_Set;

   function Set_To_Name
     (s : PC_SET) return SCALE_NAME
   is
   begin
      for name in SCALE_NAME loop
         if PCS(name) = s then
            return name;
         end if;
      end loop;
      raise Constraint_Error;
   end Set_To_Name;

end DataBase;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
