-- Seed: 12867291480888542509,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity wofkxozcoz is
  port (awreps : out std_logic; mq : out time_vector(4 to 4));
end wofkxozcoz;

architecture lnizbs of wofkxozcoz is
  
begin
  -- Single-driven assignments
  mq <= (others => 1_0_0_2_2.2_3_2_4 us);
  
  -- Multi-driven assignments
  awreps <= awreps;
  awreps <= '0';
  awreps <= awreps;
end lnizbs;

library ieee;
use ieee.std_logic_1164.all;

entity bpqlcthw is
  port (fvqqj : out integer_vector(2 to 4); xalszaj : out std_logic_vector(3 downto 4); vojuoz : inout bit);
end bpqlcthw;

library ieee;
use ieee.std_logic_1164.all;

architecture mhguh of bpqlcthw is
  signal gjwfdn : time_vector(4 to 4);
  signal mmygpgc : time_vector(4 to 4);
  signal oijkrt : time_vector(4 to 4);
  signal wwadb : std_logic;
begin
  plqq : entity work.wofkxozcoz
    port map (awreps => wwadb, mq => oijkrt);
  tehpfldrdc : entity work.wofkxozcoz
    port map (awreps => wwadb, mq => mmygpgc);
  bygyqc : entity work.wofkxozcoz
    port map (awreps => wwadb, mq => gjwfdn);
  
  -- Single-driven assignments
  vojuoz <= '0';
  
  -- Multi-driven assignments
  xalszaj <= (others => '0');
  wwadb <= 'X';
end mhguh;

library ieee;
use ieee.std_logic_1164.all;

entity ogh is
  port ( tmultg : linkage std_logic_vector(4 downto 2)
  ; mjt : inout severity_level
  ; korgdlci : out std_logic_vector(2 to 1)
  ; igdz : buffer severity_level
  );
end ogh;

library ieee;
use ieee.std_logic_1164.all;

architecture kdfcvk of ogh is
  signal yvi : time_vector(4 to 4);
  signal juutvmui : std_logic;
  signal jsmuorx : bit;
  signal odinduc : std_logic_vector(3 downto 4);
  signal r : integer_vector(2 to 4);
  signal xth : time_vector(4 to 4);
  signal qzc : time_vector(4 to 4);
  signal evnaoxudap : std_logic;
begin
  tvnldluaw : entity work.wofkxozcoz
    port map (awreps => evnaoxudap, mq => qzc);
  a : entity work.wofkxozcoz
    port map (awreps => evnaoxudap, mq => xth);
  epv : entity work.bpqlcthw
    port map (fvqqj => r, xalszaj => odinduc, vojuoz => jsmuorx);
  hfkvv : entity work.wofkxozcoz
    port map (awreps => juutvmui, mq => yvi);
  
  -- Single-driven assignments
  igdz <= ERROR;
  mjt <= igdz;
  
  -- Multi-driven assignments
  evnaoxudap <= 'H';
  korgdlci <= korgdlci;
  korgdlci <= korgdlci;
end kdfcvk;

library ieee;
use ieee.std_logic_1164.all;

entity bzxrwoix is
  port (aif : inout std_logic; syd : inout time_vector(3 downto 2));
end bzxrwoix;

library ieee;
use ieee.std_logic_1164.all;

architecture rli of bzxrwoix is
  signal beckim : time_vector(4 to 4);
  signal lztozirjz : std_logic;
  signal hqnlzk : time_vector(4 to 4);
  signal iohpas : std_logic;
  signal xq : time_vector(4 to 4);
  signal kzhvkx : std_logic;
begin
  uqjvar : entity work.wofkxozcoz
    port map (awreps => kzhvkx, mq => xq);
  eesgctf : entity work.wofkxozcoz
    port map (awreps => iohpas, mq => hqnlzk);
  msydtbndfc : entity work.wofkxozcoz
    port map (awreps => lztozirjz, mq => beckim);
  
  -- Single-driven assignments
  syd <= (2#1011.1# fs, 3 fs);
  
  -- Multi-driven assignments
  iohpas <= '1';
  aif <= lztozirjz;
end rli;



-- Seed after: 8910290022736851153,11481034001933599325
