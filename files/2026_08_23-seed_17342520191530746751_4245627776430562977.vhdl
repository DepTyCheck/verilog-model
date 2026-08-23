-- Seed: 17342520191530746751,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity wlagjcbrvn is
  port (bgswrrv : in integer_vector(3 to 2); osrqhg : out std_logic);
end wlagjcbrvn;

architecture e of wlagjcbrvn is
  
begin
  -- Multi-driven assignments
  osrqhg <= osrqhg;
  osrqhg <= osrqhg;
end e;

library ieee;
use ieee.std_logic_1164.all;

entity mrdnga is
  port (y : in boolean; vwpjp : in integer_vector(0 to 3); lhjq : buffer std_logic; bxpx : out time);
end mrdnga;

library ieee;
use ieee.std_logic_1164.all;

architecture trraynzscn of mrdnga is
  signal nkfawrw : std_logic;
  signal vgcirb : integer_vector(3 to 2);
  signal meq : std_logic;
  signal pxud : integer_vector(3 to 2);
  signal cbqiszfqsa : std_logic;
  signal fgv : integer_vector(3 to 2);
begin
  f : entity work.wlagjcbrvn
    port map (bgswrrv => fgv, osrqhg => cbqiszfqsa);
  jeimz : entity work.wlagjcbrvn
    port map (bgswrrv => pxud, osrqhg => meq);
  gdckfj : entity work.wlagjcbrvn
    port map (bgswrrv => vgcirb, osrqhg => nkfawrw);
  
  -- Single-driven assignments
  fgv <= (others => 0);
  
  -- Multi-driven assignments
  lhjq <= '-';
  meq <= '1';
  lhjq <= lhjq;
  nkfawrw <= cbqiszfqsa;
end trraynzscn;



-- Seed after: 17870569994753451102,4245627776430562977
