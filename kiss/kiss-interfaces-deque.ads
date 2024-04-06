------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

generic

   type Element_Type is private;

package Kiss.Interfaces.Deque is

   type I is interface;

   procedure Push_Front(Container: in out I; x: in Element_Type) is abstract;
   function  Pop_Front(Container: in out I) return Element_Type is abstract;
   procedure Push_Rear(Container: in out I; x: in Element_Type) is abstract;
   function  Pop_Rear(Container: in out I) return Element_Type is abstract;
   function  Front(Container: in I) return Element_Type is abstract;
   function  Rear(Container: in I) return Element_Type is abstract;
   function  Void(Container: in I) return BOOLEAN is abstract;

end Kiss.Interfaces.Deque;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
