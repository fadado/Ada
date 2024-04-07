------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Functors.Stack;
with Kiss.Functors.Queue;
with Kiss.Functors.Deque;

package body Tests is

   ---------------------------------------------------------------------
   -- Stack tests
   ---------------------------------------------------------------------

   package body Stack_I is
      procedure run_test(ADT: in out API.I'Class)
      is
      begin
         ADT.Push('Z'); 
         ADT.Push('A'); 
         if ADT.Pop /= 'A' then raise Error; end if;
         if ADT.Pop /= 'Z' then raise Error; end if;
         if not ADT.Void then raise Error; end if;
      end run_test;
   end Stack_I;

   procedure Stack_S is
      package Character_Stack is
         new Kiss.Functors.Stack (Signature);
      ADT: Character_Stack.T;
   begin
      ADT.Push('Z'); 
      ADT.Push('A'); 
      if ADT.Pop /= 'A' then raise Error; end if;
      if ADT.Pop /= 'Z' then raise Error; end if;
      if not ADT.Void then raise Error; end if;
   end Stack_S;

   ---------------------------------------------------------------------
   -- Queue tests
   ---------------------------------------------------------------------

   package body Queue_I is
      procedure run_test(ADT: in out API.I'Class)
      is
      begin
         raise Error; --TODO
      end run_test;
   end Queue_I;

   procedure Queue_S is
      package Character_Queue is
         new Kiss.Functors.Queue (Signature);
      ADT: Character_Queue.T;
   begin
      ADT.Enqueue('Z'); 
      ADT.Enqueue('A'); 
      if ADT.Dequeue /= 'Z' then raise Error; end if;
      if ADT.Dequeue /= 'A' then raise Error; end if;
      if not ADT.Void then raise Error; end if;
   end Queue_S;

   ---------------------------------------------------------------------
   -- Deque tests
   ---------------------------------------------------------------------

   package body Deque_I is
      procedure run_test(ADT: in out API.I'Class)
      is
      begin
         raise Error; --TODO
      end run_test;
   end Deque_I;

   procedure Deque_S is
      package Character_Deque is
         new Kiss.Functors.Deque (Signature);
      ADT: Character_Deque.T;
   begin
      -- as queue
      ADT.Push_Front('A'); 
      ADT.Push_Front('Z'); 
      if ADT.Pop_Rear /= 'A' then raise Error; end if;
      if ADT.Pop_Rear /= 'Z' then raise Error; end if;
      if not ADT.Void then raise Error; end if;
      -- as queue
      ADT.Push_Rear('A'); 
      ADT.Push_Rear('Z'); 
      if ADT.Pop_Front /= 'A' then raise Error; end if;
      if ADT.Pop_Front /= 'Z' then raise Error; end if;
      if not ADT.Void then raise Error; end if;
      -- as stack
      ADT.Push_Front('A'); 
      ADT.Push_Front('Z'); 
      if ADT.Pop_Front /= 'Z' then raise Error; end if;
      if ADT.Pop_Front /= 'A' then raise Error; end if;
      if not ADT.Void then raise Error; end if;
      -- as stack
      ADT.Push_Rear('A'); 
      ADT.Push_Rear('Z'); 
      if ADT.Pop_Rear /= 'Z' then raise Error; end if;
      if ADT.Pop_Rear /= 'A' then raise Error; end if;
      if not ADT.Void then raise Error; end if;
   end Deque_S;

end Tests;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
