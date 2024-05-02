------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Signatures.Queue;

generic

   with package Signature is
      new Signatures.Queue (<>);

   use Signature;
   -- type Data_Type is tagged private;
   -- type Element_Type is private;
   -- ...

package Kiss.Functors.Queue is

   type T is tagged private;

   procedure Enqueue(Container: in out T; x: in Element_Type) with Inline;
   function  Dequeue(Container: in out T) return Element_Type with Inline;
   function  Front(Container: in T) return Element_Type with Inline;
   function  Void(Container: in T) return BOOLEAN with Inline;
   function  Peek(Container: in T) return Element_Type renames Front;

private

   subtype Data_Type is Signature.Data_Type;
   type T is
      new Data_Type with null record;

end Kiss.Functors.Queue;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
