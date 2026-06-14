-- Seed: 8172177793325105484,1641934135882347475

library ieee;
use ieee.std_logic_1164.all;

entity sigx is
  port (creovarpj : inout integer_vector(2 downto 0); dnkzsxcyah : linkage bit_vector(3 to 3); qqff : linkage std_logic; nl : inout integer);
end sigx;



architecture obpwi of sigx is
  
begin
  
end obpwi;



entity ziwqks is
  port (y : linkage bit; rzq : inout integer; nw : out integer);
end ziwqks;

library ieee;
use ieee.std_logic_1164.all;

architecture uwc of ziwqks is
  signal okap : std_logic;
  signal rqdatugg : bit_vector(3 to 3);
  signal e : integer_vector(2 downto 0);
begin
  ohraxiypyr : entity work.sigx
    port map (creovarpj => e, dnkzsxcyah => rqdatugg, qqff => okap, nl => nw);
end uwc;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (wjmevlszc : buffer integer; amzkztjl : linkage std_logic_vector(3 to 1));
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture woodq of v is
  signal adxaiolbv : std_logic;
  signal amx : bit_vector(3 to 3);
  signal ryw : integer_vector(2 downto 0);
begin
  kconnvujpy : entity work.sigx
    port map (creovarpj => ryw, dnkzsxcyah => amx, qqff => adxaiolbv, nl => wjmevlszc);
end woodq;



entity zzjsxr is
  port (zufmib : inout real; jgmpu : buffer integer);
end zzjsxr;



architecture yypne of zzjsxr is
  signal kbww : integer;
  signal tlwdiecbn : bit;
begin
  kuvgm : entity work.ziwqks
    port map (y => tlwdiecbn, rzq => kbww, nw => jgmpu);
end yypne;



-- Seed after: 9456754384247721261,1641934135882347475
