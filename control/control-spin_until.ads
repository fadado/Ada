------------------------------------------------------------------------------
--  Control . Spin_Until interface
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

procedure Control . Spin_Until
  (done : access function return BOOLEAN);

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
