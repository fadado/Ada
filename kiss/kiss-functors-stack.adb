------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

package body Kiss.Functors.Stack is
   -- type Data_Type is tagged private;
   -- type Element_Type is private;
   -- ...

   procedure Push(Container: in out T; x: in Element_Type) is
      parent: Data_Type renames Data_Type(Container);
   begin
      Append(parent, x);
   end Push;

   function Pop(Container: in out T) return Element_Type is
      parent: Data_Type renames Data_Type(Container);
      result: Element_Type;
   begin
      -- require not empty
      result := Last_Element(parent);
      Delete_Last(parent);
      return result;
   end Pop;

   function Peek(Container: in T) return Element_Type is
      parent: Data_Type renames Data_Type(Container);
   begin
      -- require not empty
      return Last_Element(parent);
   end Peek;

   function void(Container: in T) return BOOLEAN is
      parent: Data_Type renames Data_Type(Container);
   begin
      return Is_Empty(parent);
   end void;

end Kiss.Functors.Stack;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
