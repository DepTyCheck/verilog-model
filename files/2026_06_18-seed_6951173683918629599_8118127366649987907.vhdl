-- Seed: 6951173683918629599,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity sjxmq is
  port (agiz : out time; mhxvfb : linkage std_logic);
end sjxmq;

architecture jbijjs of sjxmq is
  
begin
  -- Single-driven assignments
  agiz <= 8#52620.4754# us;
end jbijjs;

entity rc is
  port (apgy : linkage time_vector(1 downto 4); zkanc : buffer time; kngr : buffer bit_vector(0 downto 1); oqklf : linkage integer);
end rc;

library ieee;
use ieee.std_logic_1164.all;

architecture x of rc is
  signal lxk : time;
  signal vonslzfxm : std_logic;
  signal ska : time;
begin
  vcjyqr : entity work.sjxmq
    port map (agiz => ska, mhxvfb => vonslzfxm);
  hffqbbvjgo : entity work.sjxmq
    port map (agiz => zkanc, mhxvfb => vonslzfxm);
  iyklsq : entity work.sjxmq
    port map (agiz => lxk, mhxvfb => vonslzfxm);
  
  -- Single-driven assignments
  kngr <= (others => '0');
  
  -- Multi-driven assignments
  vonslzfxm <= 'L';
  vonslzfxm <= 'W';
end x;

library ieee;
use ieee.std_logic_1164.all;

entity kwuzlceur is
  port (pk : inout integer; lbkkgio : linkage real; iusaeicjv : buffer time; ixwp : buffer std_logic_vector(2 to 0));
end kwuzlceur;

library ieee;
use ieee.std_logic_1164.all;

architecture i of kwuzlceur is
  signal xykbks : bit_vector(0 downto 1);
  signal opsfnsq : time;
  signal enlk : time_vector(1 downto 4);
  signal kyqxyvz : integer;
  signal m : bit_vector(0 downto 1);
  signal esgkbyzgv : time_vector(1 downto 4);
  signal gdtkticox : std_logic;
  signal dqetlvwnr : time;
  signal vv : integer;
  signal xxlko : bit_vector(0 downto 1);
  signal rwhtmdeqsm : time;
  signal ndtasjr : time_vector(1 downto 4);
begin
  nodnb : entity work.rc
    port map (apgy => ndtasjr, zkanc => rwhtmdeqsm, kngr => xxlko, oqklf => vv);
  upicn : entity work.sjxmq
    port map (agiz => dqetlvwnr, mhxvfb => gdtkticox);
  s : entity work.rc
    port map (apgy => esgkbyzgv, zkanc => iusaeicjv, kngr => m, oqklf => kyqxyvz);
  sizhlpolv : entity work.rc
    port map (apgy => enlk, zkanc => opsfnsq, kngr => xykbks, oqklf => pk);
  
  -- Multi-driven assignments
  ixwp <= "";
end i;



-- Seed after: 14208160039503470576,8118127366649987907
