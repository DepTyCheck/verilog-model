-- Seed: 16547040413009694235,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity mqzd is
  port (ukerx : buffer std_logic_vector(2 downto 4); p : buffer integer; f : out integer; cthajy : buffer integer);
end mqzd;

architecture m of mqzd is
  
begin
  -- Single-driven assignments
  p <= 16#0_0_B#;
  f <= 4;
  cthajy <= 1_4_4_3;
  
  -- Multi-driven assignments
  ukerx <= "";
end m;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (umctefd : in integer; kiofll : in integer; xtzcg : inout real; mqwf : linkage std_logic);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture rarke of k is
  signal ronl : integer;
  signal zrb : integer;
  signal kekgjkxiug : integer;
  signal zobxmveh : integer;
  signal oewur : integer;
  signal tz : integer;
  signal pov : integer;
  signal llftrwt : integer;
  signal yhi : integer;
  signal ptxckbr : std_logic_vector(2 downto 4);
  signal gtw : integer;
  signal cpxdthvtm : integer;
  signal xpzqjtxplo : integer;
  signal nzqiw : std_logic_vector(2 downto 4);
begin
  zugsxprn : entity work.mqzd
    port map (ukerx => nzqiw, p => xpzqjtxplo, f => cpxdthvtm, cthajy => gtw);
  jc : entity work.mqzd
    port map (ukerx => ptxckbr, p => yhi, f => llftrwt, cthajy => pov);
  axgzsii : entity work.mqzd
    port map (ukerx => ptxckbr, p => tz, f => oewur, cthajy => zobxmveh);
  iuvkgolvi : entity work.mqzd
    port map (ukerx => nzqiw, p => kekgjkxiug, f => zrb, cthajy => ronl);
  
  -- Single-driven assignments
  xtzcg <= 8#33433.5_1_0_0#;
  
  -- Multi-driven assignments
  nzqiw <= (others => '0');
  ptxckbr <= (others => '0');
  nzqiw <= "";
end rarke;

library ieee;
use ieee.std_logic_1164.all;

entity zp is
  port (muen : out std_logic; yvurgk : inout integer; sl : buffer time);
end zp;

library ieee;
use ieee.std_logic_1164.all;

architecture grzsuxqavi of zp is
  signal utpcbiotry : std_logic;
  signal eesgby : real;
  signal xj : integer;
  signal j : integer;
  signal mmhzoltafw : integer;
  signal mdzfapkk : integer;
  signal ekdwla : std_logic_vector(2 downto 4);
  signal mefyysmaah : real;
  signal dwgrnx : integer;
  signal hrhjsaxt : integer;
begin
  tkwf : entity work.k
    port map (umctefd => hrhjsaxt, kiofll => dwgrnx, xtzcg => mefyysmaah, mqwf => muen);
  qthyaph : entity work.mqzd
    port map (ukerx => ekdwla, p => mdzfapkk, f => mmhzoltafw, cthajy => j);
  uikxmvgdkk : entity work.k
    port map (umctefd => dwgrnx, kiofll => xj, xtzcg => eesgby, mqwf => utpcbiotry);
  
  -- Single-driven assignments
  sl <= 16#9EA9# fs;
  xj <= 2#0_1_0_1_1#;
  hrhjsaxt <= 342;
  yvurgk <= 121;
  
  -- Multi-driven assignments
  muen <= '0';
  muen <= '-';
  muen <= '1';
end grzsuxqavi;



-- Seed after: 5506545351047368521,3924983747739634027
