-- Seed: 6738748001835673190,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity osrkykcrez is
  port (kfgprnyl : linkage std_logic_vector(0 downto 2); wojyruegpm : linkage std_logic);
end osrkykcrez;

architecture ojvtvrkjsx of osrkykcrez is
  
begin
  
end ojvtvrkjsx;

library ieee;
use ieee.std_logic_1164.all;

entity qu is
  port (sjt : inout integer; woqwu : buffer std_logic_vector(3 downto 4));
end qu;

library ieee;
use ieee.std_logic_1164.all;

architecture nhzweiv of qu is
  signal aoabqwb : std_logic;
  signal ocndjilgw : std_logic_vector(0 downto 2);
  signal nvcjlquj : std_logic;
begin
  tecknatln : entity work.osrkykcrez
    port map (kfgprnyl => woqwu, wojyruegpm => nvcjlquj);
  mmlli : entity work.osrkykcrez
    port map (kfgprnyl => woqwu, wojyruegpm => nvcjlquj);
  la : entity work.osrkykcrez
    port map (kfgprnyl => ocndjilgw, wojyruegpm => aoabqwb);
  
  -- Single-driven assignments
  sjt <= 3_3;
  
  -- Multi-driven assignments
  woqwu <= (others => '0');
  nvcjlquj <= nvcjlquj;
  aoabqwb <= nvcjlquj;
  aoabqwb <= 'H';
end nhzweiv;



-- Seed after: 2750551053096238689,4080032123900078489
