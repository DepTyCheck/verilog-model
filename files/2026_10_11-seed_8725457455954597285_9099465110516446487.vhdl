-- Seed: 8725457455954597285,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity uofxyl is
  port (whwajdt : linkage severity_level; gpbu : linkage std_logic_vector(0 to 2); wnywehqmcq : linkage real);
end uofxyl;

architecture kon of uofxyl is
  
begin
  
end kon;

entity xnz is
  port (akzqkfxl : in real);
end xnz;

library ieee;
use ieee.std_logic_1164.all;

architecture rkrmu of xnz is
  signal euvcxj : real;
  signal yv : std_logic_vector(0 to 2);
  signal oeze : severity_level;
begin
  xa : entity work.uofxyl
    port map (whwajdt => oeze, gpbu => yv, wnywehqmcq => euvcxj);
  
  -- Multi-driven assignments
  yv <= "ULU";
end rkrmu;

entity ororwka is
  port (cripk : linkage severity_level);
end ororwka;

library ieee;
use ieee.std_logic_1164.all;

architecture ab of ororwka is
  signal tbcw : real;
  signal doygbmrabh : real;
  signal pmud : std_logic_vector(0 to 2);
  signal ifokl : severity_level;
begin
  yyqsv : entity work.uofxyl
    port map (whwajdt => ifokl, gpbu => pmud, wnywehqmcq => doygbmrabh);
  keqzgdbm : entity work.uofxyl
    port map (whwajdt => cripk, gpbu => pmud, wnywehqmcq => tbcw);
  s : entity work.xnz
    port map (akzqkfxl => tbcw);
  
  -- Multi-driven assignments
  pmud <= "Z0-";
  pmud <= pmud;
end ab;



-- Seed after: 2385933909189196754,9099465110516446487
