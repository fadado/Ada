with Ada.Text_IO;

procedure Main is
   procedure print(line: String) renames Ada.Text_IO.Put_Line;
begin
   declare
      type ROOT is tagged
         record
            i : INTEGER := 0;
         end record;

      subtype CLASS is ROOT'Class;

      type DERIVED is new ROOT with
         record
            j : INTEGER := 0;
         end record;

      ------------------------------------------------------------------

      procedure setter(x: in out CLASS) is
      begin
         x.i := 33;
      end;

      ------------------------------------------------------------------

      a, b: ROOT;
      c: CLASS := b; -- copy
      d: DERIVED;
      e: CLASS renames CLASS(d);
   begin
      setter(a);
      print(a.i'Image);
      setter(c);
      print(b.i'Image);
      print(c.i'Image);
      --
      setter(d);
      print(d.i'Image);
      print(e.i'Image);
   end;
end Main;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
