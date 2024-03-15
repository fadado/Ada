------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Signatures.Stack;

generic
   with package Signature is new Signatures.Stack (<>);

package Kiss.Functors.Stack is

   type T is tagged private;
   subtype Element_Type is Signature.Element_Type;

   procedure Push(Container: in out T; x: in Element_Type) with Inline;
   function  Pop(Container: in out T) return Element_Type with Inline;
   function  Peek(Container: in T) return Element_Type with Inline;
   function  Is_Empty(Container: in T) return BOOLEAN with Inline;

private
   subtype Data is Signature.Data_Type;
   type T is new Data with null record;

end Kiss.Functors.Stack;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
