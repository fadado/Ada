------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Functors.Stack;

-- with package Signature is
--    new Kiss.Signatures.Stack
--      (Data_Type    => <>,
--       Element_Type => CHARACTER,
--       others       => <>);

procedure Tests.Stack_S is

   package Character_Stack is
      new Kiss.Functors.Stack (Signature);

   the_stack: Character_Stack.T;

begin

   the_stack.Push('Z'); 
   the_stack.Push('A'); 
   if the_stack.Pop /= 'A' then raise Error; end if;
   if the_stack.Pop /= 'Z' then raise Error; end if;
   if not the_stack.Void then raise Error; end if;

end Tests.Stack_S;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
