-- Seed: 1314866900203907561,8118127366649987907

entity rxnygm is
  port (ucvrvfaxuk : in real);
end rxnygm;

architecture ajyinvflcv of rxnygm is
  
begin
  
end ajyinvflcv;

library ieee;
use ieee.std_logic_1164.all;

entity sa is
  port (axawpw : out std_logic; dz : buffer real);
end sa;

architecture qrpauwm of sa is
  signal qbcyvvqvb : real;
begin
  ibdc : entity work.rxnygm
    port map (ucvrvfaxuk => dz);
  yvzev : entity work.rxnygm
    port map (ucvrvfaxuk => dz);
  s : entity work.rxnygm
    port map (ucvrvfaxuk => qbcyvvqvb);
  
  -- Single-driven assignments
  dz <= 2#1_1.0_0_1#;
  qbcyvvqvb <= 16#3D29E.3_E_3_F_B#;
  
  -- Multi-driven assignments
  axawpw <= 'W';
  axawpw <= 'U';
  axawpw <= '0';
end qrpauwm;

library ieee;
use ieee.std_logic_1164.all;

entity fpky is
  port (wtkolm : buffer std_logic; wzbklvz : linkage std_logic_vector(1 downto 1); lf : inout std_logic_vector(2 to 0));
end fpky;

library ieee;
use ieee.std_logic_1164.all;

architecture vhd of fpky is
  signal xsnrxcnw : real;
  signal vgf : std_logic;
begin
  mphiaicmph : entity work.sa
    port map (axawpw => vgf, dz => xsnrxcnw);
  
  -- Multi-driven assignments
  wtkolm <= 'L';
end vhd;



-- Seed after: 14579331589903397560,8118127366649987907
