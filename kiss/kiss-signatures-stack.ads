------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

generic
   type Data_Type is tagged private;
   type Element_Type is private;

   with procedure Append(Container: in out Data_Type; New_Item: Element_Type) is <>;
   with function  Last_Element(Container: in Data_Type) return Element_Type is <>;
   with procedure Delete_Last(Container: in out Data_Type; Count: Count_Type:=1) is <>;
   with function  Is_Empty(Container: in Data_Type) return BOOLEAN is <>;
   --  Subprograms required to implement stacks.

package Kiss.Signatures.Stack is private end;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
