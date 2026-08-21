-- Seed: 1898830622700725340,16188444798499499427

entity sykrpsff is
  port (xr : inout real; qagfj : inout real_vector(0 downto 3));
end sykrpsff;

architecture o of sykrpsff is
  
begin
  -- Single-driven assignments
  xr <= 0.4;
  qagfj <= (others => 0.0);
end o;

library ieee;
use ieee.std_logic_1164.all;

entity dkrzxuwrv is
  port (pwq : out bit; oanz : in std_logic_vector(0 downto 1); lpqxps : inout integer);
end dkrzxuwrv;

architecture pycvcwkxxd of dkrzxuwrv is
  
begin
  -- Single-driven assignments
  lpqxps <= lpqxps;
  pwq <= '1';
end pycvcwkxxd;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (sr : out std_logic);
end b;

architecture hxzrqod of b is
  
begin
  -- Multi-driven assignments
  sr <= 'U';
  sr <= sr;
  sr <= sr;
  sr <= sr;
end hxzrqod;

library ieee;
use ieee.std_logic_1164.all;

entity rdjbv is
  port (ehile : linkage std_logic_vector(1 to 3); i : linkage std_logic; cycbq : inout std_logic_vector(4 downto 1));
end rdjbv;

library ieee;
use ieee.std_logic_1164.all;

architecture appdpg of rdjbv is
  signal nntgmdkw : integer;
  signal eqqpaw : std_logic_vector(0 downto 1);
  signal twjut : bit;
  signal oo : real_vector(0 downto 3);
  signal sbgdxupf : real;
  signal pubs : std_logic;
begin
  wjfku : entity work.b
    port map (sr => pubs);
  m : entity work.sykrpsff
    port map (xr => sbgdxupf, qagfj => oo);
  uonlgrgpp : entity work.dkrzxuwrv
    port map (pwq => twjut, oanz => eqqpaw, lpqxps => nntgmdkw);
  
  -- Multi-driven assignments
  pubs <= '1';
  cycbq <= cycbq;
  pubs <= pubs;
  cycbq <= cycbq;
end appdpg;



-- Seed after: 15127112301034030640,16188444798499499427
