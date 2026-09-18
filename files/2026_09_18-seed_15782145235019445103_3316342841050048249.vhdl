-- Seed: 15782145235019445103,3316342841050048249

entity ien is
  port (ntm : out real; yzmtr : linkage severity_level; y : in integer_vector(3 to 1));
end ien;

architecture sv of ien is
  
begin
  -- Single-driven assignments
  ntm <= ntm;
end sv;

library ieee;
use ieee.std_logic_1164.all;

entity iywmo is
  port (bekaigiyr : linkage boolean; okk : out std_logic_vector(3 to 0));
end iywmo;

architecture uivij of iywmo is
  signal qiqkekyxqk : integer_vector(3 to 1);
  signal jg : severity_level;
  signal pqehzcbbvy : real;
  signal j : integer_vector(3 to 1);
  signal bcnlvq : severity_level;
  signal kupzqnfoyl : real;
begin
  z : entity work.ien
    port map (ntm => kupzqnfoyl, yzmtr => bcnlvq, y => j);
  eilmqj : entity work.ien
    port map (ntm => pqehzcbbvy, yzmtr => jg, y => qiqkekyxqk);
  
  -- Single-driven assignments
  qiqkekyxqk <= (others => 0);
  
  -- Multi-driven assignments
  okk <= okk;
  okk <= (others => '0');
  okk <= okk;
end uivij;

library ieee;
use ieee.std_logic_1164.all;

entity dtv is
  port (cbw : linkage std_logic; wguof : buffer std_logic);
end dtv;

library ieee;
use ieee.std_logic_1164.all;

architecture b of dtv is
  signal evhjtrd : std_logic_vector(3 to 0);
  signal yizfcxc : boolean;
  signal nqqc : std_logic_vector(3 to 0);
  signal gn : boolean;
  signal lkdhcch : integer_vector(3 to 1);
  signal krd : severity_level;
  signal wwtn : real;
  signal ddztho : std_logic_vector(3 to 0);
  signal qxngxgf : boolean;
begin
  fldilxip : entity work.iywmo
    port map (bekaigiyr => qxngxgf, okk => ddztho);
  qeiypthw : entity work.ien
    port map (ntm => wwtn, yzmtr => krd, y => lkdhcch);
  wcfd : entity work.iywmo
    port map (bekaigiyr => gn, okk => nqqc);
  pjja : entity work.iywmo
    port map (bekaigiyr => yizfcxc, okk => evhjtrd);
  
  -- Single-driven assignments
  lkdhcch <= (others => 0);
  
  -- Multi-driven assignments
  wguof <= wguof;
  nqqc <= ddztho;
  nqqc <= ddztho;
end b;

entity wnvj is
  port (kuzocjht : out integer);
end wnvj;

architecture z of wnvj is
  
begin
  -- Single-driven assignments
  kuzocjht <= kuzocjht;
end z;



-- Seed after: 8483176512141373661,3316342841050048249
