-- Seed: 3117762245817237567,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity lt is
  port (cxayw : in integer; yu : inout std_logic);
end lt;

architecture vxm of lt is
  
begin
  -- Multi-driven assignments
  yu <= 'Z';
  yu <= '-';
end vxm;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (yguejl : inout std_logic_vector(2 to 4); ijksx : buffer time; cbgvf : in time);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture bmhfovei of c is
  signal qmuvq : std_logic;
  signal hz : integer;
  signal nfcwkghm : std_logic;
  signal fhu : integer;
begin
  vnatdqkagx : entity work.lt
    port map (cxayw => fhu, yu => nfcwkghm);
  jjp : entity work.lt
    port map (cxayw => hz, yu => qmuvq);
  
  -- Single-driven assignments
  ijksx <= 2#0_0_0_0_0.1_1# ms;
  
  -- Multi-driven assignments
  yguejl <= ('H', 'U', 'L');
  qmuvq <= 'Z';
  nfcwkghm <= 'L';
  yguejl <= "W1W";
end bmhfovei;

library ieee;
use ieee.std_logic_1164.all;

entity alatcdxe is
  port (lrbj : inout real; wryyizeu : out std_logic_vector(2 downto 1));
end alatcdxe;

library ieee;
use ieee.std_logic_1164.all;

architecture i of alatcdxe is
  signal qcswvyi : time;
  signal xltpkpp : std_logic_vector(2 to 4);
  signal tcm : std_logic;
  signal w : integer;
begin
  akrxisyvx : entity work.lt
    port map (cxayw => w, yu => tcm);
  s : entity work.c
    port map (yguejl => xltpkpp, ijksx => qcswvyi, cbgvf => qcswvyi);
  
  -- Single-driven assignments
  lrbj <= 8#11603.4_4#;
  w <= 4;
end i;



-- Seed after: 11892448992258268385,12011142928354116943
