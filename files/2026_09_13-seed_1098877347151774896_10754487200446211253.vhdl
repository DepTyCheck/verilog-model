-- Seed: 1098877347151774896,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity jth is
  port (fbctpjfi : out character; pwe : inout real; nbszznmqn : in std_logic_vector(1 to 2); kjlakmgbe : buffer std_logic_vector(3 to 1));
end jth;

architecture duyfaswwz of jth is
  
begin
  
end duyfaswwz;

entity urzad is
  port (epyyt : in real_vector(4 to 0); v : buffer time);
end urzad;

library ieee;
use ieee.std_logic_1164.all;

architecture vuieppntxa of urzad is
  signal avtaawkkv : std_logic_vector(3 to 1);
  signal go : real;
  signal sl : character;
  signal thflwoib : std_logic_vector(3 to 1);
  signal qb : std_logic_vector(1 to 2);
  signal bhpxtzts : real;
  signal xwp : character;
  signal pbr : std_logic_vector(3 to 1);
  signal vhwqvh : std_logic_vector(1 to 2);
  signal eyrxy : real;
  signal onwqysw : character;
begin
  cdc : entity work.jth
    port map (fbctpjfi => onwqysw, pwe => eyrxy, nbszznmqn => vhwqvh, kjlakmgbe => pbr);
  okcd : entity work.jth
    port map (fbctpjfi => xwp, pwe => bhpxtzts, nbszznmqn => qb, kjlakmgbe => thflwoib);
  u : entity work.jth
    port map (fbctpjfi => sl, pwe => go, nbszznmqn => vhwqvh, kjlakmgbe => avtaawkkv);
  
  -- Single-driven assignments
  v <= 8#5# ms;
end vuieppntxa;



-- Seed after: 857821620175859323,10754487200446211253
