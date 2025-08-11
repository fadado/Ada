-- backtracker.adb

pragma Assertion_Policy(Check); -- Check / Ignore

package body Backtracker is
   solution : VECTOR_SOLUTION; -- (partial) solution

   -- Try to add one step to the partial solution
   procedure traverse
     (index : VECTOR_INDEX)
   is
      function accepted (value : NODE_VALUE) return BOOLEAN with Inline
      is
      begin
         return not Rejected(solution, index, value);
      end accepted;
   begin
      -- try to extend the solution with each choice
      for value in NODE_VALUE loop
         if accepted(value) then
            solution(index) := value;
            -- accept value for the current level

            if index /= VECTOR_INDEX'Last then
               -- recurse if solution is not completed
               Enter(solution, index, value);
               traverse(VECTOR_INDEX'Succ(index));
               Leave(solution, index, value);
            else
               Goal(solution);
            end if;
         end if;
      end loop;
   end traverse;

   -- Walk the tree prunning when a node is rejected
   procedure Traverse
     (forest : FOREST_SET := (others => TRUE))
   is
      first : constant VECTOR_INDEX := VECTOR_INDEX'First;
   begin
      for value in NODE_VALUE loop
         if forest(value) then
            solution(first) := value;

            Enter(solution, first, value);
            traverse(VECTOR_INDEX'Succ(first));
            Leave(solution, first, value);
         end if;
      end loop;
   end Traverse;
end Backtracker;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
