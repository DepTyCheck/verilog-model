-- Seed: 6473228941214832993,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity fgqyc is
  port (buhafdjc : linkage std_logic; lpe : buffer std_logic_vector(4 downto 4));
end fgqyc;

architecture t of fgqyc is
  
begin
  -- Multi-driven assignments
  lpe <= (others => 'U');
  lpe <= (others => 'L');
  lpe <= (others => 'Z');
end t;

library ieee;
use ieee.std_logic_1164.all;

entity wo is
  port (m : inout std_logic_vector(3 to 1));
end wo;

library ieee;
use ieee.std_logic_1164.all;

architecture yofx of wo is
  signal pdoxlkhwog : std_logic;
  signal udzwy : std_logic;
  signal nq : std_logic_vector(4 downto 4);
  signal yvuj : std_logic;
begin
  c : entity work.fgqyc
    port map (buhafdjc => yvuj, lpe => nq);
  y : entity work.fgqyc
    port map (buhafdjc => udzwy, lpe => nq);
  ugp : entity work.fgqyc
    port map (buhafdjc => pdoxlkhwog, lpe => nq);
  ayvxm : entity work.fgqyc
    port map (buhafdjc => yvuj, lpe => nq);
end yofx;

entity vpx is
  port (yrg : out boolean);
end vpx;

library ieee;
use ieee.std_logic_1164.all;

architecture pavxtfh of vpx is
  signal lyldzhuzhs : std_logic;
  signal ccoyeuoj : std_logic_vector(4 downto 4);
  signal x : std_logic;
  signal kszsv : std_logic_vector(4 downto 4);
  signal fy : std_logic;
begin
  etb : entity work.fgqyc
    port map (buhafdjc => fy, lpe => kszsv);
  mvtsbpv : entity work.fgqyc
    port map (buhafdjc => x, lpe => ccoyeuoj);
  gpftfhvbj : entity work.fgqyc
    port map (buhafdjc => lyldzhuzhs, lpe => ccoyeuoj);
  
  -- Single-driven assignments
  yrg <= FALSE;
  
  -- Multi-driven assignments
  fy <= '1';
  ccoyeuoj <= "W";
  fy <= '-';
  x <= 'Z';
end pavxtfh;



-- Seed after: 16096688240362913140,8421704836678237495
