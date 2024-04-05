------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Interfaces.Stack;
with Kiss.Signatures.Stack;
with Kiss.Signatures.Queue;

package Tests is

   Error : exception;

   package Stack_I is
      package IStackC is
         new Kiss.Interfaces.Stack
           (Element_Type => CHARACTER);
      procedure run_test(the_stack: in out IStackC.I'Class);
   end Stack_I;

   generic
      with package Stack_Signature is
         new Kiss.Signatures.Stack
           (Data_Type    => <>,
            Element_Type => CHARACTER,
            others       => <>);
   procedure Stack_S;

   generic
      with package Queue_Signature is
         new Kiss.Signatures.Queue
           (Data_Type    => <>,
            Element_Type => CHARACTER,
            others       => <>);
   procedure Queue;

end Tests;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
