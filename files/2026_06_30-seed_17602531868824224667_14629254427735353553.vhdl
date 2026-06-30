-- Seed: 17602531868824224667,14629254427735353553

entity ugugj is
  port (fsrzzrm : inout time);
end ugugj;

architecture betxu of ugugj is
  
begin
  -- Single-driven assignments
  fsrzzrm <= 030.23 ps;
end betxu;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (rsxmfaia : out std_logic_vector(4 to 2));
end s;

architecture tnqpyg of s is
  signal irxv : time;
  signal tolqot : time;
begin
  zjmqrpofbx : entity work.ugugj
    port map (fsrzzrm => tolqot);
  u : entity work.ugugj
    port map (fsrzzrm => irxv);
  
  -- Multi-driven assignments
  rsxmfaia <= "";
end tnqpyg;



-- Seed after: 8863863024464185610,14629254427735353553
