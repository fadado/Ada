------------------------------------------------------------------------------
--  Control . Spin_Until interface
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

procedure Control . Spin_Until
  (done : not null access function return BOOLEAN);

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
