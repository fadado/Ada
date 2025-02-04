with Music;
use  Music;

-- used to implement some tests

package DataBase is

   type SCALE_NAME is (
      Acoustic,
      Aeolian,
      Augmented_Triad,
      Chromatic,
      Diatonic,
      Diminished_7th,
      Dorian,
      Double_Harmonic,
      Harmonic_Lidyan,
      Harmonic_Major,
      Harmonic_Minor,
      Hungarian_Major_I,
      Hungarian_Major_II,
      Ionian,
      Locrian,
      Locrian_Major,
      Lydian,
      Lydian_Dominant,
      Lydian_Minor,
      Major,
      Melodic,
      Minor,
      Mixolydian,
      Monotonic,
      Octatonic,
      Pentatonic,
      Pentatonic1,
      Pentatonic2,
      Pentatonic3,
      Pentatonic4,
      Pentatonic5,
      Phrygian,
      Spanish_Phrygian,
      Tritone,
      Whole_Tone
   );

   function Name_To_Set(name: SCALE_NAME) return PC_SET;
   function Set_To_Name(set: PC_SET) return SCALE_NAME;

end DataBase;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
