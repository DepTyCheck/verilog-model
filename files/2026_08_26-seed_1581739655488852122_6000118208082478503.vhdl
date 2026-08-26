-- Seed: 1581739655488852122,6000118208082478503

entity kmieucx is
  port (pefyct : in real);
end kmieucx;

architecture t of kmieucx is
  
begin
  
end t;

library ieee;
use ieee.std_logic_1164.all;

entity uuorq is
  port (rooyuot : out std_logic_vector(1 to 1));
end uuorq;

architecture jtjbxw of uuorq is
  signal rr : real;
begin
  ie : entity work.kmieucx
    port map (pefyct => rr);
  
  -- Single-driven assignments
  rr <= rr;
  
  -- Multi-driven assignments
  rooyuot <= "L";
  rooyuot <= rooyuot;
  rooyuot <= rooyuot;
end jtjbxw;

library ieee;
use ieee.std_logic_1164.all;

entity caenp is
  port (yxvblmblyb : inout severity_level; f : buffer std_logic; eg : buffer real);
end caenp;

library ieee;
use ieee.std_logic_1164.all;

architecture u of caenp is
  signal w : std_logic_vector(1 to 1);
begin
  scybwjwkcs : entity work.kmieucx
    port map (pefyct => eg);
  z : entity work.kmieucx
    port map (pefyct => eg);
  izrpbq : entity work.uuorq
    port map (rooyuot => w);
  dxvawcfzu : entity work.kmieucx
    port map (pefyct => eg);
  
  -- Single-driven assignments
  yxvblmblyb <= FAILURE;
  eg <= 2_4_2_3_3.0_4;
  
  -- Multi-driven assignments
  f <= f;
  f <= f;
  f <= f;
end u;



-- Seed after: 1971934714377346521,6000118208082478503
