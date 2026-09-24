-- Seed: 7428082601649486908,17234720251424330329

entity cursmjze is
  port (lwepmj : linkage bit; ngmvqqp : linkage real);
end cursmjze;

architecture gxhmim of cursmjze is
  
begin
  
end gxhmim;

entity tapatmjtt is
  port (rltw : inout real);
end tapatmjtt;

architecture uhblmlpv of tapatmjtt is
  signal clhe : bit;
  signal noraxopli : real;
  signal myqzyrk : bit;
  signal wxkm : real;
  signal pmcvqghcsp : bit;
begin
  by : entity work.cursmjze
    port map (lwepmj => pmcvqghcsp, ngmvqqp => wxkm);
  iuzxmwujk : entity work.cursmjze
    port map (lwepmj => myqzyrk, ngmvqqp => noraxopli);
  ilcwt : entity work.cursmjze
    port map (lwepmj => clhe, ngmvqqp => rltw);
end uhblmlpv;

library ieee;
use ieee.std_logic_1164.all;

entity llbpfblhm is
  port (rtbzzbi : in std_logic_vector(1 to 4); baokf : inout integer);
end llbpfblhm;

architecture zllirhwk of llbpfblhm is
  signal cfpigkzh : real;
  signal fiktyb : real;
  signal m : real;
  signal gebvkrx : bit;
begin
  rzgwt : entity work.cursmjze
    port map (lwepmj => gebvkrx, ngmvqqp => m);
  tp : entity work.tapatmjtt
    port map (rltw => fiktyb);
  zuic : entity work.tapatmjtt
    port map (rltw => cfpigkzh);
end zllirhwk;

library ieee;
use ieee.std_logic_1164.all;

entity tq is
  port (eurgj : in integer_vector(4 to 1); wsy : out time; azeqva : buffer string(1 downto 1); jy : buffer std_logic);
end tq;

architecture u of tq is
  signal jhpjx : real;
  signal veaqf : bit;
  signal ikmpgvq : real;
  signal msiuooxfyt : bit;
  signal f : real;
  signal dswryfc : bit;
begin
  fitbedj : entity work.cursmjze
    port map (lwepmj => dswryfc, ngmvqqp => f);
  momskdjfm : entity work.cursmjze
    port map (lwepmj => msiuooxfyt, ngmvqqp => ikmpgvq);
  svxltu : entity work.cursmjze
    port map (lwepmj => veaqf, ngmvqqp => jhpjx);
  
  -- Single-driven assignments
  azeqva <= azeqva;
  wsy <= wsy;
  
  -- Multi-driven assignments
  jy <= 'W';
end u;



-- Seed after: 18433940942624354350,17234720251424330329
