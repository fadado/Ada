------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Signatures.Deque;

generic

   with package Signature is
      new Signatures.Deque (<>);

   use Signature;
   -- type Data_Type is tagged private;
   -- type Element_Type is private;
   -- ...

package Kiss.Functors.Deque is

   type T is tagged private;

   procedure Push_Front(Container: in out T; x: in Element_Type) with Inline;
   function  Pop_Front(Container: in out T) return Element_Type with Inline;
   procedure Push_Rear(Container: in out T; x: in Element_Type) with Inline;
   function  Pop_Rear(Container: in out T) return Element_Type with Inline;
   function  Front(Container: in T) return Element_Type with Inline;
   function  Rear(Container: in T) return Element_Type with Inline;
   function  Void(Container: in T) return BOOLEAN with Inline;

private

   subtype Data_Type is Signature.Data_Type;
   type T is new Data_Type with null record;

end Kiss.Functors.Deque;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
