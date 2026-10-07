-- Seed: 7779499225176845196,5906004015519833893

entity aosszkkyx is
  port (bgnx : out time; f : in time);
end aosszkkyx;

architecture gdrskoxdrj of aosszkkyx is
  
begin
  -- Single-driven assignments
  bgnx <= f;
end gdrskoxdrj;

library ieee;
use ieee.std_logic_1164.all;

entity uqqvvhp is
  port (wojuw : linkage severity_level; n : buffer std_logic; zpapfes : inout bit_vector(0 to 0); id : in severity_level);
end uqqvvhp;

architecture leloxqpvr of uqqvvhp is
  signal fpriccittz : time;
  signal yocyln : time;
  signal frbzd : time;
  signal mvlpsqu : time;
  signal xziqtfc : time;
begin
  mufmf : entity work.aosszkkyx
    port map (bgnx => xziqtfc, f => mvlpsqu);
  gq : entity work.aosszkkyx
    port map (bgnx => mvlpsqu, f => frbzd);
  i : entity work.aosszkkyx
    port map (bgnx => frbzd, f => xziqtfc);
  elqipuqa : entity work.aosszkkyx
    port map (bgnx => yocyln, f => fpriccittz);
  
  -- Single-driven assignments
  fpriccittz <= frbzd;
end leloxqpvr;



-- Seed after: 9728152620141355565,5906004015519833893
