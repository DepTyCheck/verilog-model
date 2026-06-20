-- Seed: 15633405512897375404,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity iivke is
  port (vfgvcdcl : out std_logic; fnsz : inout integer; ihdtx : in time_vector(1 downto 4));
end iivke;

architecture y of iivke is
  
begin
  -- Single-driven assignments
  fnsz <= 16#C#;
end y;

library ieee;
use ieee.std_logic_1164.all;

entity suqaz is
  port (gxbx : linkage std_logic; hojknhanp : buffer integer; mfxha : linkage std_logic_vector(4 downto 4));
end suqaz;

library ieee;
use ieee.std_logic_1164.all;

architecture vrs of suqaz is
  signal uf : std_logic;
  signal sptxny : integer;
  signal cxxaru : std_logic;
  signal jcrhxqthcp : time_vector(1 downto 4);
  signal av : integer;
  signal vxzq : std_logic;
begin
  k : entity work.iivke
    port map (vfgvcdcl => vxzq, fnsz => av, ihdtx => jcrhxqthcp);
  jydef : entity work.iivke
    port map (vfgvcdcl => cxxaru, fnsz => sptxny, ihdtx => jcrhxqthcp);
  yedq : entity work.iivke
    port map (vfgvcdcl => uf, fnsz => hojknhanp, ihdtx => jcrhxqthcp);
  
  -- Single-driven assignments
  jcrhxqthcp <= (others => 0 ns);
  
  -- Multi-driven assignments
  cxxaru <= '-';
end vrs;

entity tmhkmbiv is
  port (obi : buffer time_vector(1 downto 0); ghebuqrxb : out time);
end tmhkmbiv;

library ieee;
use ieee.std_logic_1164.all;

architecture wuzpe of tmhkmbiv is
  signal ef : integer;
  signal vkaf : std_logic_vector(4 downto 4);
  signal mosviey : integer;
  signal jqlugxey : time_vector(1 downto 4);
  signal rg : integer;
  signal bgf : std_logic;
begin
  rdxxeqtu : entity work.iivke
    port map (vfgvcdcl => bgf, fnsz => rg, ihdtx => jqlugxey);
  rocjt : entity work.suqaz
    port map (gxbx => bgf, hojknhanp => mosviey, mfxha => vkaf);
  frphfoyjp : entity work.iivke
    port map (vfgvcdcl => bgf, fnsz => ef, ihdtx => jqlugxey);
  
  -- Single-driven assignments
  ghebuqrxb <= 044.13 ns;
  jqlugxey <= (others => 0 ns);
  obi <= (2#10.1_1_0# us, 3 sec);
  
  -- Multi-driven assignments
  bgf <= '-';
  bgf <= 'H';
end wuzpe;



-- Seed after: 4096845764223382733,3924983747739634027
