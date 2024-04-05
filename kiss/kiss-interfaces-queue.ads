------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

generic

   type Element_Type is private;

package Kiss.Interfaces.Queue is

   type I is interface;

   procedure Enqueue(Container: in out I; x: in Element_Type) is abstract;
   function  Dequeue(Container: in out I) return Element_Type is abstract;
   function  Front(Container: in I) return Element_Type is abstract;
   function  Void(Container: in I) return BOOLEAN is abstract;
   function  Peek(Container: in I) return Element_Type renames Front;

end Kiss.Interfaces.Queue;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
