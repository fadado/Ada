------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

package body Kiss.Functors.Deque is
   -- type Data_Type is tagged private;
   -- type Element_Type is private;
   -- ...

   procedure Push_Front(Container: in out T; x: in Element_Type) is
      parent: Data_Type renames Data_Type(Container);
   begin
      Append(parent, x);
   end Push_Front;

   function Pop_Front(Container: in out T) return Element_Type is
      parent: Data_Type renames Data_Type(Container);
      result: Element_Type;
   begin
      -- require not empty
      result := Last_Element(parent);
      Delete_Last(parent);
      return result;
   end Pop_Front;

   procedure Push_Rear(Container: in out T; x: in Element_Type) is
      parent: Data_Type renames Data_Type(Container);
   begin
      Prepend(parent, x);
   end Push_Rear;

   function Pop_Rear(Container: in out T) return Element_Type is
      parent: Data_Type renames Data_Type(Container);
      result: Element_Type;
   begin
      -- require not empty
      result := First_Element(parent);
      Delete_First(parent);
      return result;
   end Pop_Rear;

   function Front(Container: in T) return Element_Type is
      parent: Data_Type renames Data_Type(Container);
   begin
      -- require not empty
      return Last_Element(parent);
   end Front;

   function Rear(Container: in T) return Element_Type is
      parent: Data_Type renames Data_Type(Container);
   begin
      -- require not empty
      return First_Element(parent);
   end Rear;

   function Void(Container: in T) return BOOLEAN is
      parent: Data_Type renames Data_Type(Container);
   begin
      return Is_Empty(parent);
   end Void;

end Kiss.Functors.Deque;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
