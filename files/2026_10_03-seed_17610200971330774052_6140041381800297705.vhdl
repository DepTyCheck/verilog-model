-- Seed: 17610200971330774052,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity ea is
  port (gsqd : buffer std_logic_vector(1 to 3); oqcyezc : out integer);
end ea;

architecture b of ea is
  
begin
  -- Single-driven assignments
  oqcyezc <= 8#731#;
  
  -- Multi-driven assignments
  gsqd <= "Z10";
  gsqd <= ('Z', 'H', '0');
  gsqd <= "HXL";
  gsqd <= "1LL";
end b;

entity clqofypjqe is
  port (f : inout boolean_vector(0 downto 0));
end clqofypjqe;

library ieee;
use ieee.std_logic_1164.all;

architecture c of clqofypjqe is
  signal mxjcbylbsv : integer;
  signal oqgyxn : std_logic_vector(1 to 3);
  signal ceeq : integer;
  signal d : integer;
  signal hpggswzxel : integer;
  signal g : std_logic_vector(1 to 3);
begin
  ib : entity work.ea
    port map (gsqd => g, oqcyezc => hpggswzxel);
  tg : entity work.ea
    port map (gsqd => g, oqcyezc => d);
  edu : entity work.ea
    port map (gsqd => g, oqcyezc => ceeq);
  rjh : entity work.ea
    port map (gsqd => oqgyxn, oqcyezc => mxjcbylbsv);
  
  -- Single-driven assignments
  f <= f;
  
  -- Multi-driven assignments
  g <= g;
  oqgyxn <= g;
  g <= "1WU";
  oqgyxn <= g;
end c;

library ieee;
use ieee.std_logic_1164.all;

entity ywxuhn is
  port (wso : linkage integer; qpmy : linkage std_logic_vector(2 to 4); qhrj : in integer);
end ywxuhn;

library ieee;
use ieee.std_logic_1164.all;

architecture iprtq of ywxuhn is
  signal ychejg : boolean_vector(0 downto 0);
  signal qsuc : integer;
  signal bgvh : integer;
  signal zeg : std_logic_vector(1 to 3);
begin
  bjjxo : entity work.ea
    port map (gsqd => zeg, oqcyezc => bgvh);
  dckrag : entity work.ea
    port map (gsqd => zeg, oqcyezc => qsuc);
  udvutd : entity work.clqofypjqe
    port map (f => ychejg);
  
  -- Multi-driven assignments
  zeg <= zeg;
  zeg <= ('X', 'X', 'U');
  zeg <= ('W', 'W', 'Z');
end iprtq;



-- Seed after: 2390165303705558775,6140041381800297705
