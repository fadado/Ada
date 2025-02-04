package body DataBase is

   PCS : constant array (SCALE_NAME) of PC_SET := (
      Acoustic =>            16#6D5#,
      Aeolian =>             16#5AD#,
      Augmented_Triad =>     16#111#,
      Chromatic =>           16#FFF#,
      Diatonic =>            16#AB5#,
      Diminished_7th =>      16#249#,
      Dorian =>              16#6AD#,
      Double_Harmonic =>     16#9B3#,
      Harmonic_Lidyan =>     16#6D5#,
      Harmonic_Major =>      16#9B5#,
      Harmonic_Minor =>      16#9AD#,
      Hungarian_Major_I =>   16#6D9#,
      Hungarian_Major_II =>  16#AD9#,
      Ionian =>              16#AB5#,
      Locrian =>             16#56B#,
      Locrian_Major =>       16#575#,
      Lydian =>              16#AD5#,
      Lydian_Dominant =>     16#6D5#,
      Lydian_Minor =>        16#5D5#,
      Major =>               16#AB5#,
      Melodic =>             16#AAD#,
      Minor =>               16#5AD#,
      Mixolydian =>          16#6B5#,
      Monotonic =>           16#001#,
      Octatonic =>           16#B6D#,
      Pentatonic =>          16#295#,
      Pentatonic1 =>         16#295#,
      Pentatonic2 =>         16#2A5#,
      Pentatonic3 =>         16#4A5#,
      Pentatonic4 =>         16#4A9#,
      Pentatonic5 =>         16#529#,
      Phrygian =>            16#5AB#,
      Spanish_Phrygian =>    16#5B3#,
      Tritone =>             16#041#,
      Whole_Tone =>          16#555#
   );

   function Name_To_Set(name: SCALE_NAME) return PC_SET is
   begin
      return PCS(name);
   end Name_To_Set;

   function Set_To_Name(set: PC_SET) return SCALE_NAME is
   begin
      for name in SCALE_NAME loop
         if PCS(name) = set then
            return name;
         end if;
      end loop;
      raise Constraint_Error;
   end Set_To_Name;

end DataBase;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
