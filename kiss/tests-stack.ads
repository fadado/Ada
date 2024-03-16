------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Signatures.Stack;

generic
   with package Signature is new Kiss.Signatures.Stack
      (Element_Type => CHARACTER,
       others       => <>);
procedure Tests.Stack;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
