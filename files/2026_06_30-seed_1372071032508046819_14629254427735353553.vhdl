-- Seed: 1372071032508046819,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity ntrnr is
  port (rddlwf : out real; jgfgtspff : in std_logic);
end ntrnr;

architecture v of ntrnr is
  
begin
  -- Single-driven assignments
  rddlwf <= 8#0_4_3.202#;
end v;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (fxkzitkz : out std_logic; dvkj : out std_logic; axbrznyzhs : in integer);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture clydzk of n is
  signal ejcy : std_logic;
  signal ahmsmwhabb : real;
  signal vcjf : real;
  signal xhteri : std_logic;
  signal c : real;
begin
  gyydfggyee : entity work.ntrnr
    port map (rddlwf => c, jgfgtspff => xhteri);
  rhggui : entity work.ntrnr
    port map (rddlwf => vcjf, jgfgtspff => dvkj);
  cbksxyd : entity work.ntrnr
    port map (rddlwf => ahmsmwhabb, jgfgtspff => ejcy);
  
  -- Multi-driven assignments
  dvkj <= 'U';
  ejcy <= '1';
  ejcy <= 'Z';
end clydzk;



-- Seed after: 1580296394233637197,14629254427735353553
