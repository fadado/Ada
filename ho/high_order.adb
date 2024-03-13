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

   package body Functors is
      package body Stack is
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
      end Stack;
   end Functors;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
