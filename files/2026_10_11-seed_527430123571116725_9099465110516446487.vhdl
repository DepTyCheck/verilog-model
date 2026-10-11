-- Seed: 527430123571116725,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity ztnkwup is
  port (ozpxno : buffer std_logic_vector(0 downto 1); inpkrsikya : out integer; ovijftpoe : in std_logic_vector(3 to 0));
end ztnkwup;

architecture rlyfksi of ztnkwup is
  
begin
  -- Single-driven assignments
  inpkrsikya <= 8#0_4_2_1_4#;
  
  -- Multi-driven assignments
  ozpxno <= ozpxno;
  ozpxno <= (others => '0');
end rlyfksi;

library ieee;
use ieee.std_logic_1164.all;

entity wxyrwcn is
  port (gggy : out std_logic_vector(3 downto 4); gwznonmd : inout real);
end wxyrwcn;

library ieee;
use ieee.std_logic_1164.all;

architecture avxfcu of wxyrwcn is
  signal yvn : integer;
  signal imd : std_logic_vector(0 downto 1);
  signal i : std_logic_vector(3 to 0);
  signal js : integer;
  signal vtfnkqi : std_logic_vector(3 to 0);
  signal edi : std_logic_vector(3 to 0);
  signal n : integer;
  signal vt : std_logic_vector(0 downto 1);
begin
  op : entity work.ztnkwup
    port map (ozpxno => vt, inpkrsikya => n, ovijftpoe => edi);
  izyz : entity work.ztnkwup
    port map (ozpxno => vtfnkqi, inpkrsikya => js, ovijftpoe => i);
  u : entity work.ztnkwup
    port map (ozpxno => imd, inpkrsikya => yvn, ovijftpoe => vtfnkqi);
  
  -- Single-driven assignments
  gwznonmd <= 2#1.101#;
end avxfcu;

library ieee;
use ieee.std_logic_1164.all;

entity ghjwlp is
  port (s : linkage real; ayqlmbgenf : buffer integer; udpwkxugr : linkage std_logic);
end ghjwlp;

architecture aiqoucuhlb of ghjwlp is
  
begin
  -- Single-driven assignments
  ayqlmbgenf <= 2_0;
end aiqoucuhlb;

library ieee;
use ieee.std_logic_1164.all;

entity iabfphtytj is
  port (kwfsovv : out time; becyg : inout severity_level; warowk : buffer std_logic_vector(2 to 2); y : linkage real);
end iabfphtytj;

library ieee;
use ieee.std_logic_1164.all;

architecture tkmuswj of iabfphtytj is
  signal itawuniuh : integer;
  signal a : std_logic_vector(0 downto 1);
  signal mxxourrg : std_logic_vector(3 to 0);
  signal qdgmdu : integer;
  signal nzbeatpn : real;
  signal jxbryxftr : std_logic_vector(3 to 0);
begin
  aprlknse : entity work.wxyrwcn
    port map (gggy => jxbryxftr, gwznonmd => nzbeatpn);
  wkmvwfeq : entity work.ztnkwup
    port map (ozpxno => jxbryxftr, inpkrsikya => qdgmdu, ovijftpoe => mxxourrg);
  esqloeyc : entity work.ztnkwup
    port map (ozpxno => a, inpkrsikya => itawuniuh, ovijftpoe => jxbryxftr);
  
  -- Single-driven assignments
  becyg <= FAILURE;
  kwfsovv <= kwfsovv;
  
  -- Multi-driven assignments
  a <= (others => '0');
  warowk <= "-";
  mxxourrg <= jxbryxftr;
end tkmuswj;



-- Seed after: 946858967691101316,9099465110516446487
