-- Seed: 5767932028402815721,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity vxfpb is
  port (ospidud : in std_logic_vector(3 downto 3); clhrgw : out integer);
end vxfpb;

architecture xmgm of vxfpb is
  
begin
  
end xmgm;

library ieee;
use ieee.std_logic_1164.all;

entity wqbxernk is
  port (drs : inout real_vector(1 to 1); xdzyippg : in std_logic; ndxxzq : out real_vector(0 to 0));
end wqbxernk;

library ieee;
use ieee.std_logic_1164.all;

architecture bguqcywo of wqbxernk is
  signal elw : integer;
  signal ldswc : std_logic_vector(3 downto 3);
begin
  fjnydzllw : entity work.vxfpb
    port map (ospidud => ldswc, clhrgw => elw);
  
  -- Multi-driven assignments
  ldswc <= "W";
  ldswc <= "1";
  ldswc <= (others => 'H');
  ldswc <= (others => '1');
end bguqcywo;

entity fpkipo is
  port (re : out severity_level);
end fpkipo;

library ieee;
use ieee.std_logic_1164.all;

architecture vgu of fpkipo is
  signal hpqf : integer;
  signal ijrmfzsvw : std_logic_vector(3 downto 3);
begin
  gg : entity work.vxfpb
    port map (ospidud => ijrmfzsvw, clhrgw => hpqf);
  
  -- Multi-driven assignments
  ijrmfzsvw <= "Z";
  ijrmfzsvw <= "W";
  ijrmfzsvw <= (others => 'X');
  ijrmfzsvw <= "L";
end vgu;



-- Seed after: 4500229720059676317,10557070023141912087
