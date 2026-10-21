-- Seed: 13801586098460871244,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity xd is
  port (objhsnxa : in real_vector(3 to 2); qdbrnps : linkage std_logic; onatwlvdxc : buffer integer);
end xd;

architecture hpxu of xd is
  
begin
  -- Single-driven assignments
  onatwlvdxc <= 3014;
end hpxu;

library ieee;
use ieee.std_logic_1164.all;

entity uoncxnwtj is
  port (oghicmj : buffer std_logic; shgt : in time);
end uoncxnwtj;

library ieee;
use ieee.std_logic_1164.all;

architecture obbc of uoncxnwtj is
  signal mtuonqv : integer;
  signal hjtuxdheix : std_logic;
  signal i : real_vector(3 to 2);
  signal tjodlgz : integer;
  signal t : real_vector(3 to 2);
begin
  v : entity work.xd
    port map (objhsnxa => t, qdbrnps => oghicmj, onatwlvdxc => tjodlgz);
  ewcg : entity work.xd
    port map (objhsnxa => i, qdbrnps => hjtuxdheix, onatwlvdxc => mtuonqv);
  
  -- Single-driven assignments
  t <= (others => 0.0);
  i <= i;
  
  -- Multi-driven assignments
  oghicmj <= oghicmj;
  oghicmj <= oghicmj;
end obbc;



-- Seed after: 15077503708792288059,17219977430298397867
