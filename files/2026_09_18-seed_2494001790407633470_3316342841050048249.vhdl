-- Seed: 2494001790407633470,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity bkcp is
  port (jmk : out std_logic; ymtyhspatl : in bit_vector(4 downto 0));
end bkcp;

architecture empnsgind of bkcp is
  
begin
  -- Multi-driven assignments
  jmk <= 'W';
  jmk <= 'L';
  jmk <= jmk;
end empnsgind;

library ieee;
use ieee.std_logic_1164.all;

entity dnjvcw is
  port (wjppamcf : out std_logic; hawvxbhvq : in time; pnfhelk : in severity_level);
end dnjvcw;

architecture cvfx of dnjvcw is
  
begin
  -- Multi-driven assignments
  wjppamcf <= '-';
end cvfx;

library ieee;
use ieee.std_logic_1164.all;

entity ynmzlrwu is
  port (gkiwfaghl : buffer std_logic; zohxd : in real; sgxaek : buffer std_logic_vector(1 downto 0); utryjg : inout time);
end ynmzlrwu;

library ieee;
use ieee.std_logic_1164.all;

architecture ccv of ynmzlrwu is
  signal vbzb : bit_vector(4 downto 0);
  signal nuoutw : std_logic;
  signal quusso : bit_vector(4 downto 0);
  signal yrldn : std_logic;
  signal t : severity_level;
begin
  kxps : entity work.dnjvcw
    port map (wjppamcf => gkiwfaghl, hawvxbhvq => utryjg, pnfhelk => t);
  yl : entity work.dnjvcw
    port map (wjppamcf => yrldn, hawvxbhvq => utryjg, pnfhelk => t);
  gjlw : entity work.bkcp
    port map (jmk => gkiwfaghl, ymtyhspatl => quusso);
  qw : entity work.bkcp
    port map (jmk => nuoutw, ymtyhspatl => vbzb);
  
  -- Single-driven assignments
  t <= FAILURE;
end ccv;



-- Seed after: 611715472167848906,3316342841050048249
