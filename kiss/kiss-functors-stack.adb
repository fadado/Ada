------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

package body Kiss.Functors.Stack is

   use Signature;

   procedure Push(Container: in out T; x: in Element_Type) is
      C: Data renames Data(Container);
   begin
      Append(C, x);
   end Push;

   function Pop(Container: in out T) return Element_Type is
      C: Data renames Data(Container);
      result: Element_Type;
   begin
      -- require not empty
      result := Last_Element(C);
      Delete_Last(C);
      return result;
   end Pop;

   function Peek(Container: in T) return Element_Type is
      C: Data renames Data(Container);
   begin
      -- require not empty
      return Last_Element(C);
   end Peek;

   function Is_Empty(Container: in T) return BOOLEAN is
      C: Data renames Data(Container);
   begin
      return Is_Empty(C);
   end Is_Empty;

end Kiss.Functors.Stack;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
