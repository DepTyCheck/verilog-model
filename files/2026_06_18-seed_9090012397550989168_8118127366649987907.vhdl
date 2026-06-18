-- Seed: 9090012397550989168,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity avwhphy is
  port (vgndm : inout time; nmhzyybd : in std_logic; scaqaupi : out std_logic; mtjshl : out character);
end avwhphy;

architecture u of avwhphy is
  
begin
  -- Single-driven assignments
  mtjshl <= 'g';
  vgndm <= 14110.0_4_1 ns;
end u;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (ywhonqa : out std_logic; lnpxgqkzaf : linkage integer_vector(1 downto 4); dw : buffer std_logic_vector(0 downto 2); eiajh : buffer real);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture tirk of i is
  signal eth : character;
  signal sywdpyz : std_logic;
  signal jgdjkuvpr : time;
  signal dcnadigpid : character;
  signal ikmvbx : std_logic;
  signal dvr : time;
  signal rhtp : character;
  signal hpv : time;
begin
  jbybqvve : entity work.avwhphy
    port map (vgndm => hpv, nmhzyybd => ywhonqa, scaqaupi => ywhonqa, mtjshl => rhtp);
  eqbfq : entity work.avwhphy
    port map (vgndm => dvr, nmhzyybd => ikmvbx, scaqaupi => ywhonqa, mtjshl => dcnadigpid);
  ixrcjm : entity work.avwhphy
    port map (vgndm => jgdjkuvpr, nmhzyybd => sywdpyz, scaqaupi => ywhonqa, mtjshl => eth);
  
  -- Single-driven assignments
  eiajh <= 16#16F3D.2_F_6#;
  
  -- Multi-driven assignments
  dw <= (others => '0');
end tirk;



-- Seed after: 9651583536593815958,8118127366649987907
