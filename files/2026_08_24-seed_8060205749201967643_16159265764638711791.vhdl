-- Seed: 8060205749201967643,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity mr is
  port (zzdueswsc : inout std_logic_vector(4 to 1));
end mr;

architecture zkpszut of mr is
  
begin
  -- Multi-driven assignments
  zzdueswsc <= "";
  zzdueswsc <= (others => '0');
end zkpszut;

library ieee;
use ieee.std_logic_1164.all;

entity bhmkoiffc is
  port (qxkasiccc : buffer std_logic_vector(2 to 3); xmbcut : inout integer);
end bhmkoiffc;

library ieee;
use ieee.std_logic_1164.all;

architecture wj of bhmkoiffc is
  signal gplammiov : std_logic_vector(4 to 1);
  signal yafahfyknn : std_logic_vector(4 to 1);
  signal ckpdzbj : std_logic_vector(4 to 1);
begin
  roaqmqd : entity work.mr
    port map (zzdueswsc => ckpdzbj);
  fdwyfxdbsy : entity work.mr
    port map (zzdueswsc => ckpdzbj);
  azqv : entity work.mr
    port map (zzdueswsc => yafahfyknn);
  t : entity work.mr
    port map (zzdueswsc => gplammiov);
  
  -- Multi-driven assignments
  ckpdzbj <= "";
end wj;

entity dsbkxach is
  port (sabnq : in time; tzpyp : linkage integer);
end dsbkxach;

library ieee;
use ieee.std_logic_1164.all;

architecture yftckuxu of dsbkxach is
  signal coo : integer;
  signal ifwefqc : std_logic_vector(2 to 3);
begin
  z : entity work.bhmkoiffc
    port map (qxkasiccc => ifwefqc, xmbcut => coo);
  
  -- Multi-driven assignments
  ifwefqc <= ('X', 'U');
  ifwefqc <= ('U', 'Z');
end yftckuxu;

library ieee;
use ieee.std_logic_1164.all;

entity esctai is
  port (gpkeqnmq : out std_logic);
end esctai;

library ieee;
use ieee.std_logic_1164.all;

architecture n of esctai is
  signal uk : integer;
  signal zntciayfa : std_logic_vector(2 to 3);
  signal oroulkztsm : std_logic_vector(4 to 1);
begin
  bqemtdxl : entity work.mr
    port map (zzdueswsc => oroulkztsm);
  trcgcuarcf : entity work.mr
    port map (zzdueswsc => oroulkztsm);
  dsvrwkwaat : entity work.mr
    port map (zzdueswsc => oroulkztsm);
  vatt : entity work.bhmkoiffc
    port map (qxkasiccc => zntciayfa, xmbcut => uk);
  
  -- Multi-driven assignments
  oroulkztsm <= oroulkztsm;
end n;



-- Seed after: 18082806137138908668,16159265764638711791
