-- Seed: 13112172361018684710,5472058987609252853

entity agbtnfow is
  port (rpvxdj : buffer integer; nkkw : out boolean; wjevivba : out time; u : in integer);
end agbtnfow;

architecture g of agbtnfow is
  
begin
  
end g;

library ieee;
use ieee.std_logic_1164.all;

entity kjgnxuj is
  port (mtkqadd : linkage std_logic);
end kjgnxuj;

architecture dbfkfetph of kjgnxuj is
  signal bkzybwc : time;
  signal fubjncgpp : boolean;
  signal bywoxg : integer;
  signal paomxcf : time;
  signal fqmam : boolean;
  signal qqhi : integer;
begin
  ak : entity work.agbtnfow
    port map (rpvxdj => qqhi, nkkw => fqmam, wjevivba => paomxcf, u => bywoxg);
  qe : entity work.agbtnfow
    port map (rpvxdj => bywoxg, nkkw => fubjncgpp, wjevivba => bkzybwc, u => qqhi);
end dbfkfetph;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (cncyqfc : out std_logic);
end v;

architecture fbbznrrmni of v is
  signal dd : integer;
  signal ywiaxj : time;
  signal xprqfvkc : boolean;
  signal kbut : integer;
begin
  a : entity work.kjgnxuj
    port map (mtkqadd => cncyqfc);
  mkymg : entity work.agbtnfow
    port map (rpvxdj => kbut, nkkw => xprqfvkc, wjevivba => ywiaxj, u => dd);
  
  -- Multi-driven assignments
  cncyqfc <= '1';
  cncyqfc <= 'W';
  cncyqfc <= 'L';
  cncyqfc <= 'W';
end fbbznrrmni;



-- Seed after: 12338708445333250938,5472058987609252853
