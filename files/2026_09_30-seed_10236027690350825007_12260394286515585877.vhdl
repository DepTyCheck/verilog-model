-- Seed: 10236027690350825007,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity ldasykku is
  port (oglocfimd : inout std_logic; rlbnzwdd : linkage character; wkqhylnc : out std_logic_vector(1 to 2); gyvlfqawr : linkage integer);
end ldasykku;

architecture lhfzlvi of ldasykku is
  
begin
  -- Multi-driven assignments
  wkqhylnc <= wkqhylnc;
  wkqhylnc <= ('-', 'H');
  wkqhylnc <= ('X', 'H');
end lhfzlvi;

library ieee;
use ieee.std_logic_1164.all;

entity rdufi is
  port (rqwzovkllh : linkage bit; hhlmh : inout std_logic);
end rdufi;

library ieee;
use ieee.std_logic_1164.all;

architecture zgxkp of rdufi is
  signal dvluo : integer;
  signal hpwyw : character;
  signal azuucdmpg : integer;
  signal nclljbpk : std_logic_vector(1 to 2);
  signal stf : character;
  signal noajuspywj : std_logic;
begin
  ybebmn : entity work.ldasykku
    port map (oglocfimd => noajuspywj, rlbnzwdd => stf, wkqhylnc => nclljbpk, gyvlfqawr => azuucdmpg);
  kkjartzp : entity work.ldasykku
    port map (oglocfimd => hhlmh, rlbnzwdd => hpwyw, wkqhylnc => nclljbpk, gyvlfqawr => dvluo);
  
  -- Multi-driven assignments
  noajuspywj <= hhlmh;
  noajuspywj <= 'U';
  nclljbpk <= "01";
end zgxkp;



-- Seed after: 1459549187198728112,12260394286515585877
