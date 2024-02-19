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
   -- Data structures
   ---------------------------------------------------------------------

   package body G_Stack_1 is
      procedure Push(self: in out T; x: in ELEMENT_TYPE) is
      begin
         self.Append(x);
      end Push;

      function Pop(self: in out T) return ELEMENT_TYPE is
         result: ELEMENT_TYPE;
      begin
         -- require no empty
         result := self.Last_Element;
         self.Delete_Last;
         return result;
      end Pop;

      function Void(self: in T) return BOOLEAN is
      begin
         return self.Is_Empty;
      end Void;
   end G_Stack_1;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
