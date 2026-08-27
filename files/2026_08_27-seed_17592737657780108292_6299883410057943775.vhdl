-- Seed: 17592737657780108292,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity unjs is
  port (yxjmj : in std_logic);
end unjs;

architecture kf of unjs is
  
begin
  
end kf;

library ieee;
use ieee.std_logic_1164.all;

entity poxh is
  port (bykbz : buffer real; odl : inout std_logic; iwhuryj : out std_logic_vector(4 to 0));
end poxh;

library ieee;
use ieee.std_logic_1164.all;

architecture miwpaexa of poxh is
  signal grnjiy : std_logic;
begin
  q : entity work.unjs
    port map (yxjmj => odl);
  yjo : entity work.unjs
    port map (yxjmj => odl);
  ienai : entity work.unjs
    port map (yxjmj => grnjiy);
  
  -- Single-driven assignments
  bykbz <= bykbz;
  
  -- Multi-driven assignments
  iwhuryj <= iwhuryj;
  iwhuryj <= "";
  grnjiy <= grnjiy;
  iwhuryj <= "";
end miwpaexa;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (gso : in std_logic);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture wggc of x is
  signal rdhesmhuou : std_logic_vector(4 to 0);
  signal redm : std_logic;
  signal teqt : real;
begin
  tsdhd : entity work.poxh
    port map (bykbz => teqt, odl => redm, iwhuryj => rdhesmhuou);
  gkgxrgsz : entity work.unjs
    port map (yxjmj => redm);
  
  -- Multi-driven assignments
  redm <= gso;
end wggc;

library ieee;
use ieee.std_logic_1164.all;

entity acsi is
  port (xwponztnh : inout boolean_vector(2 to 4); y : inout time; sxhvhs : inout integer; hjm : linkage std_logic);
end acsi;

architecture vdnxbiaa of acsi is
  
begin
  -- Single-driven assignments
  y <= y;
  xwponztnh <= (TRUE, FALSE, TRUE);
  sxhvhs <= sxhvhs;
end vdnxbiaa;



-- Seed after: 18159948076781244029,6299883410057943775
