------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

generic

   type Element_Type is private;

package Kiss.Interfaces.Stack is

   type I is interface;

   procedure Push(Container: in out I; x: in Element_Type) is abstract;
   function  Pop(Container: in out I) return Element_Type is abstract;
   function  Top(Container: in I) return Element_Type is abstract;
   function  Void(Container: in I) return BOOLEAN is abstract;
   function  Peek(Container: in I) return Element_Type renames Top;

end Kiss.Interfaces.Stack;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
