-- Seed: 3758666030564184887,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity xttzmqgwse is
  port (txjsfkehpi : inout std_logic; mdjwqfx : out integer);
end xttzmqgwse;

architecture vy of xttzmqgwse is
  
begin
  -- Single-driven assignments
  mdjwqfx <= 8#2#;
end vy;

entity azdj is
  port (z : inout integer);
end azdj;

library ieee;
use ieee.std_logic_1164.all;

architecture x of azdj is
  signal zpln : integer;
  signal qchmfdfy : std_logic;
begin
  fipa : entity work.xttzmqgwse
    port map (txjsfkehpi => qchmfdfy, mdjwqfx => z);
  exnu : entity work.xttzmqgwse
    port map (txjsfkehpi => qchmfdfy, mdjwqfx => zpln);
  
  -- Multi-driven assignments
  qchmfdfy <= '0';
end x;



-- Seed after: 13632429457255604386,6882842853887419669
