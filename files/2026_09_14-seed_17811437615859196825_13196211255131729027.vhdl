-- Seed: 17811437615859196825,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity ibjcamig is
  port (tjrvtvyl : in severity_level; p : inout integer; tjlobez : inout std_logic);
end ibjcamig;

architecture aeg of ibjcamig is
  
begin
  -- Single-driven assignments
  p <= 16#D_E_F_5_E#;
  
  -- Multi-driven assignments
  tjlobez <= tjlobez;
  tjlobez <= 'X';
  tjlobez <= 'X';
end aeg;

library ieee;
use ieee.std_logic_1164.all;

entity jbklnxtmp is
  port (zb : in std_logic);
end jbklnxtmp;

library ieee;
use ieee.std_logic_1164.all;

architecture aiw of jbklnxtmp is
  signal ytiub : std_logic;
  signal hzkvmd : integer;
  signal sf : severity_level;
  signal ccaitx : std_logic;
  signal zzt : integer;
  signal bdgz : std_logic;
  signal vlxbbyvktg : integer;
  signal dnf : severity_level;
begin
  zir : entity work.ibjcamig
    port map (tjrvtvyl => dnf, p => vlxbbyvktg, tjlobez => bdgz);
  z : entity work.ibjcamig
    port map (tjrvtvyl => dnf, p => zzt, tjlobez => ccaitx);
  gpw : entity work.ibjcamig
    port map (tjrvtvyl => sf, p => hzkvmd, tjlobez => ytiub);
  
  -- Single-driven assignments
  dnf <= WARNING;
  sf <= WARNING;
  
  -- Multi-driven assignments
  bdgz <= zb;
  ccaitx <= zb;
end aiw;



-- Seed after: 1985835863214686590,13196211255131729027
