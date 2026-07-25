-- Seed: 2617143820433165146,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity ugvfca is
  port (iaznuxklm : linkage boolean_vector(0 to 4); lwoawwx : out time; m : in std_logic);
end ugvfca;

architecture inwxuc of ugvfca is
  
begin
  -- Single-driven assignments
  lwoawwx <= lwoawwx;
end inwxuc;

library ieee;
use ieee.std_logic_1164.all;

entity ifahkx is
  port (ksxglgjcs : linkage time; xl : linkage integer; shchrgi : linkage std_logic_vector(1 to 4));
end ifahkx;

library ieee;
use ieee.std_logic_1164.all;

architecture x of ifahkx is
  signal im : time;
  signal vbzsyccxib : boolean_vector(0 to 4);
  signal dqj : std_logic;
  signal bru : time;
  signal stfqrpkw : boolean_vector(0 to 4);
begin
  wizbcltz : entity work.ugvfca
    port map (iaznuxklm => stfqrpkw, lwoawwx => bru, m => dqj);
  qse : entity work.ugvfca
    port map (iaznuxklm => vbzsyccxib, lwoawwx => im, m => dqj);
  
  -- Multi-driven assignments
  dqj <= 'Z';
  dqj <= dqj;
  dqj <= 'L';
  dqj <= 'H';
end x;

library ieee;
use ieee.std_logic_1164.all;

entity oolk is
  port (seim : buffer std_logic; bokscdagi : linkage integer; hy : out time);
end oolk;

architecture ayvaxws of oolk is
  signal qhrvct : time;
  signal th : boolean_vector(0 to 4);
begin
  rnvkc : entity work.ugvfca
    port map (iaznuxklm => th, lwoawwx => qhrvct, m => seim);
  
  -- Multi-driven assignments
  seim <= 'X';
  seim <= seim;
end ayvaxws;

library ieee;
use ieee.std_logic_1164.all;

entity vgjqnck is
  port (e : inout std_logic_vector(3 downto 3); uczfccj : inout bit);
end vgjqnck;

library ieee;
use ieee.std_logic_1164.all;

architecture ouuskrwft of vgjqnck is
  signal iozpvrk : time;
  signal qfyzgxzzi : boolean_vector(0 to 4);
  signal hmykc : std_logic_vector(1 to 4);
  signal jffundjjio : integer;
  signal dcv : time;
  signal drujt : time;
  signal felo : integer;
  signal eoob : std_logic;
  signal ffnlixc : std_logic;
  signal jhgyaa : time;
  signal do : boolean_vector(0 to 4);
begin
  yhtmcweddj : entity work.ugvfca
    port map (iaznuxklm => do, lwoawwx => jhgyaa, m => ffnlixc);
  voul : entity work.oolk
    port map (seim => eoob, bokscdagi => felo, hy => drujt);
  j : entity work.ifahkx
    port map (ksxglgjcs => dcv, xl => jffundjjio, shchrgi => hmykc);
  xiuxseu : entity work.ugvfca
    port map (iaznuxklm => qfyzgxzzi, lwoawwx => iozpvrk, m => eoob);
  
  -- Single-driven assignments
  uczfccj <= '1';
  
  -- Multi-driven assignments
  e <= "1";
  hmykc <= hmykc;
  e <= e;
  e <= (others => '-');
end ouuskrwft;



-- Seed after: 11568043525231911394,5306691039457971049
