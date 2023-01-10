-- oop.adb

with Ada.Text_IO;

-- Root <- Shape <- Circle

procedure OOP is
------------------------------------------------------------------------
   package Root is
------------------------------------------------------------------------
      type    INNER is abstract tagged private;
      subtype CLASS is INNER'Class;
      type    PROXY is access all CLASS;
      -- experiments
      function Level(self: in INNER) return INTEGER with Inline;
   private
      type INNER is abstract tagged null record;
   end Root;
   package body Root is
      -- experiments
      function Level(self: in INNER) return INTEGER is
      begin
         return 0;
      end;
   end Root;

------------------------------------------------------------------------
   package Shape is
------------------------------------------------------------------------
      subtype UPPER is Root.INNER;
      type    INNER is new UPPER with private;
      subtype CLASS is INNER'Class;
      type    PROXY is access all CLASS;
      --
      procedure Set_X(self: in out INNER; X: in INTEGER) with Inline;
      procedure Set_Y(self: in out INNER; Y: in INTEGER) with Inline;
      function  Get_X(self: in INNER) return INTEGER with Inline;
      function  Get_Y(self: in INNER) return INTEGER with Inline;
      --
      function Level(self: in INNER) return INTEGER with Inline;
   private
      type INNER is new UPPER with
         record
            X, Y : INTEGER := 0;
         end record;
   end Shape;
   package body Shape is
      procedure Set_X(self: in out INNER; X: in INTEGER) is
      begin
         self.X := X;
      end Set_X;
      function Get_X(self: in INNER) return INTEGER is
      begin
         return self.X;
      end Get_X;
      procedure Set_Y(self: in out INNER; Y: in INTEGER) is
      begin
         self.Y := Y;
      end Set_Y;
      function Get_Y(self: in INNER) return INTEGER is
      begin
         return self.Y;
      end Get_Y;
      --
      function Level(self: in INNER) return INTEGER is
         super : UPPER renames UPPER(self);
      begin
         return 1 + super.Level;
      end;
   end Shape;

------------------------------------------------------------------------
   package Circle is
------------------------------------------------------------------------
      subtype UPPER is Shape.INNER;
      type    INNER is new UPPER with private;
      subtype CLASS is INNER'Class;
      type    PROXY is access all CLASS;
      --
      function Level(self: in INNER) return INTEGER with Inline;
   private
      type INNER is new UPPER with
         record
            R : INTEGER := 1;
         end record;
   end Circle;
   package body Circle is
      --
      function Level(self: in INNER) return INTEGER is
         super : UPPER renames UPPER(self);
      begin
         return 1 + super.Level;
      end;
   end Circle;
------------------------------------------------------------------------
begin
   MAIN:
      declare
         procedure print(str: STRING) renames Ada.Text_IO.put_line;
         a : Circle.INNER;
      begin
         a.Set_X(33);
         print("X =" & a.Get_X'Image);
         print("Y =" & a.Get_Y'Image);
         print("L =" & a.Level'Image);
      end MAIN;
end OOP;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
