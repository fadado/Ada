------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Signatures.Stack;

generic

   with package Signature is
      new Signatures.Stack (<>);

   use Signature;
   -- type Data_Type is tagged private;
   -- type Element_Type is private;
   -- ...

package Kiss.Functors.Stack is

   type T is tagged private;

   procedure Push(Container: in out T; x: in Element_Type) with Inline;
   function  Pop(Container: in out T) return Element_Type with Inline;
   function  Top(Container: in T) return Element_Type with Inline;
   function  Void(Container: in T) return BOOLEAN with Inline;
   function  Peek(Container: in T) return Element_Type renames Top;

private

   subtype Data_Type is Signature.Data_Type;
   type T is new Data_Type with null record;

end Kiss.Functors.Stack;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
