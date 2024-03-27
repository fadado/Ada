------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

generic

   type Element_Type is private;

package Kiss.Interfaces.Stack is

   type T is interface;

   procedure Push(Container: in out T; x: in Element_Type) is abstract;
   function  Pop(Container: in out T) return Element_Type is abstract;
   function  Peek(Container: in T) return Element_Type is abstract;
   function  Is_Empty(Container: in T) return BOOLEAN is abstract;

end Kiss.Interfaces.Stack;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
