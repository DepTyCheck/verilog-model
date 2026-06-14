-- Seed: 1728861214953097982,1641934135882347475



entity rdjfnlwby is
  port (c : in real; pqi : inout character);
end rdjfnlwby;



architecture diiwac of rdjfnlwby is
  
begin
  
end diiwac;

library ieee;
use ieee.std_logic_1164.all;

entity zxs is
  port (rgpmj : out integer; dvgj : inout std_logic_vector(0 downto 1); jafgzffulj : in real_vector(1 to 2));
end zxs;



architecture htufhzmvuz of zxs is
  signal pbajkp : character;
  signal uya : character;
  signal weq : real;
  signal otjw : character;
  signal rt : real;
  signal walog : character;
  signal u : real;
begin
  varg : entity work.rdjfnlwby
    port map (c => u, pqi => walog);
  mmwhlktu : entity work.rdjfnlwby
    port map (c => rt, pqi => otjw);
  uqoagddn : entity work.rdjfnlwby
    port map (c => weq, pqi => uya);
  cmjezfsvyr : entity work.rdjfnlwby
    port map (c => u, pqi => pbajkp);
end htufhzmvuz;



entity hi is
  port (lv : out boolean_vector(2 to 1); rztbx : in real);
end hi;

library ieee;
use ieee.std_logic_1164.all;

architecture bdxrrjyo of hi is
  signal pn : character;
  signal tsl : character;
  signal pehdvngam : real_vector(1 to 2);
  signal mytvpc : std_logic_vector(0 downto 1);
  signal zqbv : integer;
begin
  ykn : entity work.zxs
    port map (rgpmj => zqbv, dvgj => mytvpc, jafgzffulj => pehdvngam);
  kvc : entity work.rdjfnlwby
    port map (c => rztbx, pqi => tsl);
  nbaej : entity work.rdjfnlwby
    port map (c => rztbx, pqi => pn);
end bdxrrjyo;



entity dtvhn is
  port (ffnwqi : inout severity_level; efc : inout character);
end dtvhn;



architecture iqbwyguz of dtvhn is
  signal yt : character;
  signal wu : character;
  signal zrpijqg : real;
  signal b : boolean_vector(2 to 1);
begin
  tcx : entity work.hi
    port map (lv => b, rztbx => zrpijqg);
  cis : entity work.rdjfnlwby
    port map (c => zrpijqg, pqi => wu);
  jfc : entity work.rdjfnlwby
    port map (c => zrpijqg, pqi => yt);
end iqbwyguz;



-- Seed after: 8802136657720658344,1641934135882347475
