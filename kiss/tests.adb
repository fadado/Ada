------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Functors.Stack;
with Kiss.Functors.Queue;

package body Tests is

   package body Stack_I is
      procedure run_test(the_stack: in out IStackC.I'Class)
      is
      begin
         the_stack.Push('Z'); 
         the_stack.Push('A'); 
         if the_stack.Pop /= 'A' then raise Error; end if;
         if the_stack.Pop /= 'Z' then raise Error; end if;
         if not the_stack.Void then raise Error; end if;
      end run_test;
   end Stack_I;

   procedure Stack_S is
      package Character_Stack is
         new Kiss.Functors.Stack (Stack_Signature);
      the_stack: Character_Stack.T;
   begin
      the_stack.Push('Z'); 
      the_stack.Push('A'); 
      if the_stack.Pop /= 'A' then raise Error; end if;
      if the_stack.Pop /= 'Z' then raise Error; end if;
      if not the_stack.Void then raise Error; end if;
   end Stack_S;

   package body Queue_I is
      procedure run_test(the_queue: in out IQueueC.I'Class)
      is
      begin
         the_queue.Enqueue('Z'); 
         the_queue.Enqueue('A'); 
         if the_queue.Dequeue /= 'Z' then raise Error; end if;
         if the_queue.Dequeue /= 'A' then raise Error; end if;
         if not the_queue.Void then raise Error; end if;
      end run_test;
   end Queue_I;

   procedure Queue_S is
      package Character_Queue is
         new Kiss.Functors.Queue (Queue_Signature);
      the_queue: Character_Queue.T;
   begin
      the_queue.Enqueue('Z'); 
      the_queue.Enqueue('A'); 
      if the_queue.Dequeue /= 'Z' then raise Error; end if;
      if the_queue.Dequeue /= 'A' then raise Error; end if;
      if not the_queue.Void then raise Error; end if;
   end Queue_S;

end Tests;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
