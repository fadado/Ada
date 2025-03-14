package Music.Names is

   C  : constant PITCH_CLASS :=  0;
   Cs : constant PITCH_CLASS :=  1;
   Db : constant PITCH_CLASS :=  1;
   D  : constant PITCH_CLASS :=  2;
   Ds : constant PITCH_CLASS :=  3;
   Eb : constant PITCH_CLASS :=  3;
   E  : constant PITCH_CLASS :=  4;
   F  : constant PITCH_CLASS :=  5;
   Fs : constant PITCH_CLASS :=  6;
   Gb : constant PITCH_CLASS :=  6;
   G  : constant PITCH_CLASS :=  7;
   Gs : constant PITCH_CLASS :=  8;
   Ab : constant PITCH_CLASS :=  8;
   A  : constant PITCH_CLASS :=  9;
   As : constant PITCH_CLASS := 10;
   Bb : constant PITCH_CLASS := 10;
   B  : constant PITCH_CLASS := 11;

   Unison    : constant PC_INTERVAL :=  0;
   Minor_2   : constant PC_INTERVAL :=  1;
   Major_2   : constant PC_INTERVAL :=  2;
   Minor_3   : constant PC_INTERVAL :=  3;
   Major_3   : constant PC_INTERVAL :=  4;
   Perfect_4 : constant PC_INTERVAL :=  5;
   Tritone   : constant PC_INTERVAL :=  6;
   Perfect_5 : constant PC_INTERVAL :=  7;
   Minor_6   : constant PC_INTERVAL :=  8;
   Major_6   : constant PC_INTERVAL :=  9;
   Minor_7   : constant PC_INTERVAL := 10;
   Major_7   : constant PC_INTERVAL := 11;

   subtype DYAD         is PC_TUPLE(1..2);
   subtype TRICHORD     is PC_TUPLE(1..3);
   subtype TETRACHORD   is PC_TUPLE(1..4);
   subtype PENTACHORD   is PC_TUPLE(1..5);
   subtype HEXACHORD    is PC_TUPLE(1..6);
   subtype HEPTACHORD   is PC_TUPLE(1..7);
   subtype OCTACHORD    is PC_TUPLE(1..8);
   subtype NONACHORD    is PC_TUPLE(1..9);
   subtype DECACHORD    is PC_TUPLE(1..10);
   subtype UNDECACHORD  is PC_TUPLE(1..11);
   subtype DODECACHORD  is PC_TUPLE(1..12);

   subtype TONE_ROW     is DODECACHORD;
   subtype SERIES       is DODECACHORD;

end Music.Names;
-- �ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
