-- Seed: 16189062982261860307,511364357853360275

entity rttemh is
  port (g : buffer time; z : linkage boolean_vector(1 downto 0));
end rttemh;

architecture ypyhgy of rttemh is
  
begin
  -- Single-driven assignments
  g <= 141 ps;
end ypyhgy;

library ieee;
use ieee.std_logic_1164.all;

entity uk is
  port (jbwzbx : in std_logic; zyhedgbmas : linkage time; gxeqkosez : in std_logic; ariipnsm : buffer std_logic_vector(4 to 3));
end uk;

architecture q of uk is
  signal er : boolean_vector(1 downto 0);
  signal xx : time;
begin
  ggfifv : entity work.rttemh
    port map (g => xx, z => er);
end q;

entity alksdpwy is
  port (o : linkage integer);
end alksdpwy;

library ieee;
use ieee.std_logic_1164.all;

architecture eh of alksdpwy is
  signal nxh : boolean_vector(1 downto 0);
  signal ewbpqxqrpt : time;
  signal aawfftnroq : std_logic_vector(4 to 3);
  signal yenjyltasf : time;
  signal ohvfawc : std_logic;
begin
  cpcf : entity work.uk
    port map (jbwzbx => ohvfawc, zyhedgbmas => yenjyltasf, gxeqkosez => ohvfawc, ariipnsm => aawfftnroq);
  dazitqr : entity work.rttemh
    port map (g => ewbpqxqrpt, z => nxh);
  
  -- Multi-driven assignments
  ohvfawc <= ohvfawc;
end eh;



-- Seed after: 13342985585159678327,511364357853360275
