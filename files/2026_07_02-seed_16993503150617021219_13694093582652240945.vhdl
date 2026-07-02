-- Seed: 16993503150617021219,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity iqvad is
  port (ltslotdf : linkage std_logic; iq : inout integer; abzlltt : out std_logic_vector(4 downto 4); ndprce : out real);
end iqvad;

architecture vf of iqvad is
  
begin
  -- Single-driven assignments
  ndprce <= 41.42033;
  iq <= 3_1;
  
  -- Multi-driven assignments
  abzlltt <= "1";
  abzlltt <= "H";
end vf;

library ieee;
use ieee.std_logic_1164.all;

entity vazyzqgd is
  port (tpvxet : out time; i : buffer integer; k : out std_logic);
end vazyzqgd;

library ieee;
use ieee.std_logic_1164.all;

architecture heefsp of vazyzqgd is
  signal gnd : real;
  signal nd : integer;
  signal pubz : real;
  signal mlu : integer;
  signal nddmlxat : real;
  signal t : std_logic_vector(4 downto 4);
  signal hcsorg : std_logic;
begin
  vmta : entity work.iqvad
    port map (ltslotdf => hcsorg, iq => i, abzlltt => t, ndprce => nddmlxat);
  goi : entity work.iqvad
    port map (ltslotdf => k, iq => mlu, abzlltt => t, ndprce => pubz);
  rntmglvuv : entity work.iqvad
    port map (ltslotdf => k, iq => nd, abzlltt => t, ndprce => gnd);
  
  -- Single-driven assignments
  tpvxet <= 16#C# fs;
  
  -- Multi-driven assignments
  k <= 'U';
  t <= (others => 'W');
  t <= "Z";
end heefsp;



-- Seed after: 2137367570435953889,13694093582652240945
