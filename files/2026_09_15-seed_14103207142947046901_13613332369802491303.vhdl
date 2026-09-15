-- Seed: 14103207142947046901,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity vxv is
  port (hmrbjx : linkage std_logic_vector(4 downto 0));
end vxv;

architecture b of vxv is
  
begin
  
end b;

entity j is
  port (bdrltho : in boolean; rkemj : inout real);
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture frlj of j is
  signal xiclcxz : std_logic_vector(4 downto 0);
  signal im : std_logic_vector(4 downto 0);
begin
  okpnckw : entity work.vxv
    port map (hmrbjx => im);
  qpfid : entity work.vxv
    port map (hmrbjx => im);
  q : entity work.vxv
    port map (hmrbjx => xiclcxz);
  
  -- Single-driven assignments
  rkemj <= rkemj;
  
  -- Multi-driven assignments
  xiclcxz <= ('0', 'Z', 'L', 'W', '0');
  im <= xiclcxz;
  im <= xiclcxz;
  xiclcxz <= ('Z', 'H', '-', '-', '1');
end frlj;



-- Seed after: 15941130634100107159,13613332369802491303
