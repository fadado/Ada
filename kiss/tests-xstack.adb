------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

procedure Tests.XStack is

   the_stack: Character_Stack.T;

begin
   the_stack.Push('Z'); 
   the_stack.Push('A'); 
   if the_stack.Pop /= 'A' then raise Error; end if;
   if the_stack.Pop /= 'Z' then raise Error; end if;
   if not the_stack.Is_Empty then raise Error; end if;

end Tests.XStack;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
