-- Seed: 155989548079676645,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity yy is
  port (o : inout std_logic; wvli : buffer real; lgv : out time);
end yy;

architecture vhn of yy is
  
begin
  -- Single-driven assignments
  lgv <= 16#A# ps;
  wvli <= 16#8_0_F_3_8.0F5A#;
  
  -- Multi-driven assignments
  o <= o;
  o <= o;
end vhn;

library ieee;
use ieee.std_logic_1164.all;

entity fno is
  port (q : out real; xu : in bit; jprqbrgs : out std_logic_vector(0 downto 2); mab : in std_logic);
end fno;

library ieee;
use ieee.std_logic_1164.all;

architecture biudnlokmw of fno is
  signal slyv : time;
  signal xcwesfh : std_logic;
begin
  ixtcfp : entity work.yy
    port map (o => xcwesfh, wvli => q, lgv => slyv);
  
  -- Multi-driven assignments
  jprqbrgs <= (others => '0');
  jprqbrgs <= jprqbrgs;
  jprqbrgs <= jprqbrgs;
end biudnlokmw;



-- Seed after: 14660499003984310898,8437298063418820479
