-- Seed: 9555326463368965092,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity zyxpkbcz is
  port (gkqfsigx : out boolean_vector(3 to 0); drsmeqw : linkage std_logic);
end zyxpkbcz;

architecture xb of zyxpkbcz is
  
begin
  -- Single-driven assignments
  gkqfsigx <= (others => TRUE);
end xb;

entity qewen is
  port (qkhz : linkage integer; jjezi : out boolean; pzkrlgyt : inout integer; lvaz : inout boolean_vector(1 to 1));
end qewen;

library ieee;
use ieee.std_logic_1164.all;

architecture suvajwijys of qewen is
  signal kyqt : std_logic;
  signal ubyvox : boolean_vector(3 to 0);
  signal kdvbz : std_logic;
  signal dipqawzr : boolean_vector(3 to 0);
  signal vudggbyemr : std_logic;
  signal kz : boolean_vector(3 to 0);
begin
  tp : entity work.zyxpkbcz
    port map (gkqfsigx => kz, drsmeqw => vudggbyemr);
  farajekauh : entity work.zyxpkbcz
    port map (gkqfsigx => dipqawzr, drsmeqw => kdvbz);
  xjedrqb : entity work.zyxpkbcz
    port map (gkqfsigx => ubyvox, drsmeqw => kyqt);
  
  -- Single-driven assignments
  lvaz <= (others => FALSE);
  jjezi <= jjezi;
  pzkrlgyt <= pzkrlgyt;
  
  -- Multi-driven assignments
  vudggbyemr <= 'L';
end suvajwijys;



-- Seed after: 16181722676942941376,3400751927341804175
