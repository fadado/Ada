------------------------------------------------------------------------
-- High Order Imperative Programming
------------------------------------------------------------------------
package body High_Order is
   ---------------------------------------------------------------------
   -- Generic subprograms
   ---------------------------------------------------------------------

   procedure G_Swap(x, y: in out DEFINITE) is
      z : constant DEFINITE := x;
   begin
      x := y;
      y := z;
   end G_Swap;

   function G_Compose(x: A) return A is
   begin
      return F(G(x));
   end G_Compose;

   function G_Compose_3(x: A) return C is
   begin
      return F(G(x));
   end G_Compose_3;

   ---------------------------------------------------------------------
   -- Data PARENTs
   ---------------------------------------------------------------------

   package body Functors is
      package body Stack is
         use Signature;

         procedure Push(self: in out T; x: in ELEMENT_TYPE) is
            super: PARENT renames PARENT(self);
         begin
            Append(super, x);
         end Push;

         function Pop(self: in out T) return ELEMENT_TYPE is
            super: PARENT renames PARENT(self);
            result: ELEMENT_TYPE;
         begin
            -- require not empty
            result := Last_Element(super);
            Delete_Last(super);
            return result;
         end Pop;

         function Void(self: in T) return BOOLEAN is
            super: PARENT renames PARENT(self);
         begin
            return Is_Empty(super);
         end Void;
      end Stack;
   end Functors;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
