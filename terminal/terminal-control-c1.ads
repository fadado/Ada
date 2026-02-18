private with Terminal.Control.C0;

------------------------------------------------------------------------
private package Terminal.Control.C1 is
------------------------------------------------------------------------
   pragma Pure(C1);
   CSI   : constant STRING := (C0.ESC, '[');
   HTS   : constant STRING := (C0.ESC, 'H');
   NEL   : constant STRING := (C0.ESC, 'E');
   OSC   : constant STRING := (C0.ESC, ']');
   RI    : constant STRING := (C0.ESC, 'M');
   ST    : constant STRING := (C0.ESC, '\');
end Terminal.Control.C1;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
