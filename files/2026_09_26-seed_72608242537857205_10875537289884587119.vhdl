-- Seed: 72608242537857205,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity xypmryaew is
  port (nwlx : buffer real; zblaoqr : out std_logic_vector(1 downto 3); qeuz : inout integer_vector(1 to 1));
end xypmryaew;

architecture uyihhe of xypmryaew is
  
begin
  -- Multi-driven assignments
  zblaoqr <= zblaoqr;
end uyihhe;

entity ggaka is
  port (bgimdembdr : out bit_vector(1 to 4); mmjs : inout bit_vector(4 to 1));
end ggaka;

library ieee;
use ieee.std_logic_1164.all;

architecture wivdb of ggaka is
  signal rlgcaveo : integer_vector(1 to 1);
  signal wl : std_logic_vector(1 downto 3);
  signal hxparojwm : real;
begin
  ezez : entity work.xypmryaew
    port map (nwlx => hxparojwm, zblaoqr => wl, qeuz => rlgcaveo);
  
  -- Single-driven assignments
  mmjs <= mmjs;
  bgimdembdr <= ('0', '1', '0', '0');
end wivdb;

entity r is
  port (lsswknedg : in integer_vector(1 downto 3));
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture yqz of r is
  signal dmzd : integer_vector(1 to 1);
  signal h : std_logic_vector(1 downto 3);
  signal plqikcntgf : real;
begin
  bqfgsfmyo : entity work.xypmryaew
    port map (nwlx => plqikcntgf, zblaoqr => h, qeuz => dmzd);
end yqz;

entity fu is
  port (ag : inout time; qxwocuoetq : inout time);
end fu;

library ieee;
use ieee.std_logic_1164.all;

architecture rggalb of fu is
  signal nobspjd : integer_vector(1 to 1);
  signal vbnyjfhlw : std_logic_vector(1 downto 3);
  signal ozwts : real;
  signal pkoj : integer_vector(1 to 1);
  signal pk : std_logic_vector(1 downto 3);
  signal rcmwfh : real;
  signal pesi : integer_vector(1 downto 3);
begin
  djhakoa : entity work.r
    port map (lsswknedg => pesi);
  dgncowwndr : entity work.xypmryaew
    port map (nwlx => rcmwfh, zblaoqr => pk, qeuz => pkoj);
  wiyjtl : entity work.xypmryaew
    port map (nwlx => ozwts, zblaoqr => vbnyjfhlw, qeuz => nobspjd);
  tj : entity work.r
    port map (lsswknedg => pesi);
  
  -- Single-driven assignments
  qxwocuoetq <= 8#0_2.45542# ns;
  
  -- Multi-driven assignments
  pk <= (others => '0');
  pk <= (others => '0');
  pk <= (others => '0');
  pk <= pk;
end rggalb;



-- Seed after: 7067230884715099652,10875537289884587119
