-- Seed: 10325606226632952724,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity dk is
  port (cewps : out std_logic);
end dk;

architecture jp of dk is
  
begin
  -- Multi-driven assignments
  cewps <= cewps;
  cewps <= cewps;
  cewps <= cewps;
  cewps <= cewps;
end jp;

entity dfcme is
  port (bvhcurucqj : buffer severity_level);
end dfcme;

library ieee;
use ieee.std_logic_1164.all;

architecture iypzdbj of dfcme is
  signal zcnfvjtavb : std_logic;
  signal jvnmswpp : std_logic;
begin
  yilaokwjsg : entity work.dk
    port map (cewps => jvnmswpp);
  zsmm : entity work.dk
    port map (cewps => jvnmswpp);
  rat : entity work.dk
    port map (cewps => zcnfvjtavb);
  
  -- Single-driven assignments
  bvhcurucqj <= WARNING;
  
  -- Multi-driven assignments
  jvnmswpp <= jvnmswpp;
  jvnmswpp <= jvnmswpp;
  jvnmswpp <= zcnfvjtavb;
  jvnmswpp <= 'W';
end iypzdbj;



-- Seed after: 6738748001835673190,4080032123900078489
