-- Seed: 17801860270516016727,6562834012285671529

entity f is
  port (dbsbadi : linkage real; hzkmid : inout time);
end f;

architecture yyvtvvga of f is
  
begin
  -- Single-driven assignments
  hzkmid <= 3 min;
end yyvtvvga;

library ieee;
use ieee.std_logic_1164.all;

entity ccigmvga is
  port (wcsbm : buffer std_logic_vector(3 downto 4); uyf : inout real; dfbkrt : linkage integer);
end ccigmvga;

architecture xwdpkdtuy of ccigmvga is
  signal abjs : time;
  signal rdnyma : real;
begin
  kckwj : entity work.f
    port map (dbsbadi => rdnyma, hzkmid => abjs);
  
  -- Single-driven assignments
  uyf <= uyf;
  
  -- Multi-driven assignments
  wcsbm <= "";
  wcsbm <= wcsbm;
end xwdpkdtuy;

library ieee;
use ieee.std_logic_1164.all;

entity yfvot is
  port (xiroex : buffer std_logic_vector(0 to 4); snkie : linkage real; aag : buffer integer; vdazqibay : buffer real_vector(2 downto 2));
end yfvot;

library ieee;
use ieee.std_logic_1164.all;

architecture txlznyeqx of yfvot is
  signal gpre : real;
  signal d : std_logic_vector(3 downto 4);
begin
  jmnxdgttu : entity work.ccigmvga
    port map (wcsbm => d, uyf => gpre, dfbkrt => aag);
end txlznyeqx;

library ieee;
use ieee.std_logic_1164.all;

entity zvw is
  port (iueofznh : buffer real; apzv : out std_logic_vector(3 to 1));
end zvw;

library ieee;
use ieee.std_logic_1164.all;

architecture lnjhlp of zvw is
  signal pccy : time;
  signal bwnd : real_vector(2 downto 2);
  signal urdjryb : integer;
  signal dalbe : real;
  signal xe : std_logic_vector(0 to 4);
  signal lskmcvanjm : integer;
  signal xjzhpiuq : real;
  signal twcroo : std_logic_vector(3 downto 4);
begin
  tameq : entity work.ccigmvga
    port map (wcsbm => twcroo, uyf => xjzhpiuq, dfbkrt => lskmcvanjm);
  vbg : entity work.yfvot
    port map (xiroex => xe, snkie => dalbe, aag => urdjryb, vdazqibay => bwnd);
  nt : entity work.f
    port map (dbsbadi => iueofznh, hzkmid => pccy);
  
  -- Multi-driven assignments
  xe <= xe;
  apzv <= apzv;
  apzv <= "";
end lnjhlp;



-- Seed after: 7516357495749416734,6562834012285671529
