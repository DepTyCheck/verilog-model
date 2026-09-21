-- Seed: 7586525496565064852,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity vukcney is
  port (zl : buffer std_logic_vector(2 to 2); sxzskrlm : linkage std_logic_vector(0 downto 0));
end vukcney;

architecture yk of vukcney is
  
begin
  -- Multi-driven assignments
  zl <= (others => '0');
  zl <= (others => '1');
  zl <= zl;
  zl <= zl;
end yk;

library ieee;
use ieee.std_logic_1164.all;

entity llq is
  port (gxlxrjfs : buffer std_logic_vector(0 downto 2); ep : buffer std_logic_vector(0 downto 3); zbhqub : inout real; f : out std_logic);
end llq;

architecture b of llq is
  
begin
  -- Single-driven assignments
  zbhqub <= zbhqub;
end b;

entity gighoxcbtp is
  port (rolh : buffer integer);
end gighoxcbtp;

library ieee;
use ieee.std_logic_1164.all;

architecture yljvio of gighoxcbtp is
  signal yss : std_logic_vector(0 downto 0);
  signal evybovrbi : std_logic_vector(0 downto 0);
  signal ynlxx : std_logic;
  signal zcz : real;
  signal ntaly : std_logic_vector(0 downto 3);
begin
  ldwt : entity work.llq
    port map (gxlxrjfs => ntaly, ep => ntaly, zbhqub => zcz, f => ynlxx);
  zmr : entity work.vukcney
    port map (zl => evybovrbi, sxzskrlm => yss);
  pivj : entity work.vukcney
    port map (zl => evybovrbi, sxzskrlm => evybovrbi);
  
  -- Single-driven assignments
  rolh <= 032;
  
  -- Multi-driven assignments
  ynlxx <= 'X';
  ntaly <= (others => '0');
  ntaly <= ntaly;
end yljvio;



-- Seed after: 11728553530792919624,12143220691580258643
