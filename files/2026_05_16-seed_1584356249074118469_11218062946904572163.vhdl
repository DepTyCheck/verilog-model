-- Seed: 1584356249074118469,11218062946904572163

library ieee;
use ieee.std_logic_1164.all;

entity qo is
  port (idxvbxqzaj : out std_logic; bokxj : out real; ejxdulgrw : buffer std_logic);
end qo;



architecture bsdt of qo is
  
begin
  
end bsdt;

library ieee;
use ieee.std_logic_1164.all;

entity docxpxazb is
  port (vmzlfg : out integer; iqpisse : buffer severity_level; wljxsre : inout std_logic);
end docxpxazb;

library ieee;
use ieee.std_logic_1164.all;

architecture okxnhmzq of docxpxazb is
  signal pczzxmj : std_logic;
  signal hag : real;
  signal rvwjqhapl : real;
begin
  askieso : entity work.qo
    port map (idxvbxqzaj => wljxsre, bokxj => rvwjqhapl, ejxdulgrw => wljxsre);
  l : entity work.qo
    port map (idxvbxqzaj => wljxsre, bokxj => hag, ejxdulgrw => pczzxmj);
end okxnhmzq;

library ieee;
use ieee.std_logic_1164.all;

entity dqmxpxs is
  port (lp : buffer std_logic);
end dqmxpxs;

library ieee;
use ieee.std_logic_1164.all;

architecture rlltw of dqmxpxs is
  signal iiiclyeivs : std_logic;
  signal zrvtnyr : severity_level;
  signal bsg : integer;
  signal isvz : severity_level;
  signal zllfz : integer;
  signal znbsp : real;
begin
  milfjvbapt : entity work.qo
    port map (idxvbxqzaj => lp, bokxj => znbsp, ejxdulgrw => lp);
  k : entity work.docxpxazb
    port map (vmzlfg => zllfz, iqpisse => isvz, wljxsre => lp);
  tbmssmfs : entity work.docxpxazb
    port map (vmzlfg => bsg, iqpisse => zrvtnyr, wljxsre => iiiclyeivs);
end rlltw;

library ieee;
use ieee.std_logic_1164.all;

entity hvgtuicr is
  port (hxjggpfy : inout time; o : inout integer; rvfnl : out integer; pdwsb : linkage std_logic);
end hvgtuicr;

library ieee;
use ieee.std_logic_1164.all;

architecture xdsa of hvgtuicr is
  signal xkr : severity_level;
  signal grcipm : std_logic;
  signal da : severity_level;
  signal vsuknp : std_logic;
begin
  n : entity work.dqmxpxs
    port map (lp => vsuknp);
  dlvfaba : entity work.docxpxazb
    port map (vmzlfg => rvfnl, iqpisse => da, wljxsre => grcipm);
  syjjn : entity work.docxpxazb
    port map (vmzlfg => o, iqpisse => xkr, wljxsre => vsuknp);
  xcgzp : entity work.dqmxpxs
    port map (lp => vsuknp);
end xdsa;



-- Seed after: 4696037150805807136,11218062946904572163
