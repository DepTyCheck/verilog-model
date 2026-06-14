-- Seed: 4784071492448345468,1852660963590380551

library ieee;
use ieee.std_logic_1164.all;

entity xwt is
  port (x : buffer std_logic_vector(1 downto 1); tvstystojr : inout std_logic; exnotmsld : buffer std_logic_vector(2 downto 1); jwpw : in integer);
end xwt;



architecture dcvdcvumpj of xwt is
  
begin
  
end dcvdcvumpj;



entity og is
  port (ntcbyook : in time);
end og;

library ieee;
use ieee.std_logic_1164.all;

architecture qwpky of og is
  signal ujsl : std_logic;
  signal jiysyognv : std_logic_vector(2 downto 1);
  signal bhahfbzvf : std_logic;
  signal ifdqwexuvn : std_logic_vector(2 downto 1);
  signal r : integer;
  signal jnnnreu : std_logic_vector(2 downto 1);
  signal opg : std_logic;
  signal tseudusx : std_logic_vector(1 downto 1);
begin
  twsf : entity work.xwt
    port map (x => tseudusx, tvstystojr => opg, exnotmsld => jnnnreu, jwpw => r);
  n : entity work.xwt
    port map (x => tseudusx, tvstystojr => opg, exnotmsld => ifdqwexuvn, jwpw => r);
  qfnbk : entity work.xwt
    port map (x => tseudusx, tvstystojr => bhahfbzvf, exnotmsld => jiysyognv, jwpw => r);
  vwvonsjn : entity work.xwt
    port map (x => tseudusx, tvstystojr => ujsl, exnotmsld => jiysyognv, jwpw => r);
end qwpky;



entity nprrvmbh is
  port (wz : out time);
end nprrvmbh;

library ieee;
use ieee.std_logic_1164.all;

architecture fzril of nprrvmbh is
  signal fjx : time;
  signal tgzmbvicd : time;
  signal fwjfvwrqm : integer;
  signal z : std_logic_vector(2 downto 1);
  signal kvdapi : std_logic;
  signal fyukzmuv : std_logic_vector(1 downto 1);
begin
  r : entity work.xwt
    port map (x => fyukzmuv, tvstystojr => kvdapi, exnotmsld => z, jwpw => fwjfvwrqm);
  rcwtskd : entity work.og
    port map (ntcbyook => tgzmbvicd);
  fqedi : entity work.og
    port map (ntcbyook => fjx);
end fzril;



-- Seed after: 7512705366031565252,1852660963590380551
