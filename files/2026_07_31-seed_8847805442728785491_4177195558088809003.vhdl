-- Seed: 8847805442728785491,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity mxhhqstqv is
  port (bb : out std_logic; u : out std_logic; fajw : in time);
end mxhhqstqv;

architecture q of mxhhqstqv is
  
begin
  -- Multi-driven assignments
  bb <= u;
  u <= u;
end q;

library ieee;
use ieee.std_logic_1164.all;

entity zcdr is
  port (nqbtczdnyq : inout bit; rpkygbj : inout integer; qbtjkijbej : buffer real; g : inout std_logic);
end zcdr;

library ieee;
use ieee.std_logic_1164.all;

architecture n of zcdr is
  signal obc : time;
  signal yfxdokm : std_logic;
  signal fw : std_logic;
  signal dlckvrm : time;
begin
  l : entity work.mxhhqstqv
    port map (bb => g, u => g, fajw => dlckvrm);
  ypgke : entity work.mxhhqstqv
    port map (bb => fw, u => g, fajw => dlckvrm);
  wyucbgzwad : entity work.mxhhqstqv
    port map (bb => g, u => fw, fajw => dlckvrm);
  unojl : entity work.mxhhqstqv
    port map (bb => g, u => yfxdokm, fajw => obc);
  
  -- Single-driven assignments
  rpkygbj <= rpkygbj;
  
  -- Multi-driven assignments
  g <= 'L';
  g <= g;
  g <= '1';
  g <= g;
end n;



-- Seed after: 1373547640210144420,4177195558088809003
