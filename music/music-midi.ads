-- An example implementation for pitches encoded as in MIDI.
-- Several other encodings are possible.

package Music.MIDI is

   type    PITCH is range 0..127;
   type    PITCH_INTERVAL is range -127..+127;
   subtype UNORDERED_INTERVAL is PITCH_INTERVAL range 0..127;

   -- Ordered pitch interval
   function Interval(x, y: PITCH) return PITCH_INTERVAL
      is (PITCH_INTERVAL(y - x)) with Inline;

   -- Pich transposition
   function Transposition(i: PITCH_INTERVAL; x: PITCH) return PITCH
      is (x + PITCH'Base(i)) with Inline;

   -- Unordered pitch interval
   -- "abs"(i: PITCH_INTERVAL): UNORDERED_INTERVAL
   --     abs Interval(x, y)

end Music.MIDI;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
