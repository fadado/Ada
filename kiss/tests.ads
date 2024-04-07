------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Interfaces.Stack;
with Kiss.Signatures.Stack;
with Kiss.Interfaces.Queue;
with Kiss.Signatures.Queue;
with Kiss.Interfaces.Deque;
with Kiss.Signatures.Deque;

package Tests is

   Error : exception;

   ---------------------------------------------------------------------
   -- Stack tests
   ---------------------------------------------------------------------

   package Stack_I is
      package API is
         new Kiss.Interfaces.Stack
           (Element_Type => CHARACTER);

      procedure run_test(ADT: in out API.I'Class);
   end Stack_I;

   generic
      with package Signature is
         new Kiss.Signatures.Stack
           (Data_Type    => <>,
            Element_Type => CHARACTER,
            others       => <>);
   procedure Stack_S;

   ---------------------------------------------------------------------
   -- Queue tests
   ---------------------------------------------------------------------

   package Queue_I is
      package API is
         new Kiss.Interfaces.Queue
           (Element_Type => CHARACTER);

      procedure run_test(ADT: in out API.I'Class);
   end Queue_I;

   generic
      with package Signature is
         new Kiss.Signatures.Queue
           (Data_Type    => <>,
            Element_Type => CHARACTER,
            others       => <>);
   procedure Queue_S;

   ---------------------------------------------------------------------
   -- Deque tests
   ---------------------------------------------------------------------

   package Deque_I is
      package API is
         new Kiss.Interfaces.Deque
           (Element_Type => CHARACTER);

      procedure run_test(ADT: in out API.I'Class);
   end Deque_I;

   generic
      with package Signature is
         new Kiss.Signatures.Deque
           (Data_Type    => <>,
            Element_Type => CHARACTER,
            others       => <>);
   procedure Deque_S;

end Tests;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
