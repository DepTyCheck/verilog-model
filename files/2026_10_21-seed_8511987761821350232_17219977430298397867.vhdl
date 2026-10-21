-- Seed: 8511987761821350232,17219977430298397867

entity mipg is
  port (ebkajafw : inout time; qozrza : buffer time);
end mipg;

architecture cqzfufklvh of mipg is
  
begin
  -- Single-driven assignments
  qozrza <= 2#1_0.0_0_1_0# ps;
  ebkajafw <= qozrza;
end cqzfufklvh;

library ieee;
use ieee.std_logic_1164.all;

entity cni is
  port (qvch : inout bit; plv : buffer std_logic_vector(4 downto 4));
end cni;

architecture suwh of cni is
  
begin
  -- Single-driven assignments
  qvch <= qvch;
  
  -- Multi-driven assignments
  plv <= (others => 'Z');
  plv <= (others => 'L');
  plv <= "-";
  plv <= "-";
end suwh;

library ieee;
use ieee.std_logic_1164.all;

entity uquldjr is
  port (ifzdal : inout std_logic_vector(2 to 4); bgrmrhrp : buffer real_vector(3 to 2));
end uquldjr;

architecture syiomivtm of uquldjr is
  signal tpxcqul : time;
  signal onsen : time;
  signal db : time;
  signal eeqbv : time;
  signal bf : time;
  signal lswtjy : time;
  signal c : time;
  signal yqi : time;
begin
  stqzapf : entity work.mipg
    port map (ebkajafw => yqi, qozrza => c);
  z : entity work.mipg
    port map (ebkajafw => lswtjy, qozrza => bf);
  rlvwd : entity work.mipg
    port map (ebkajafw => eeqbv, qozrza => db);
  a : entity work.mipg
    port map (ebkajafw => onsen, qozrza => tpxcqul);
  
  -- Single-driven assignments
  bgrmrhrp <= (others => 0.0);
  
  -- Multi-driven assignments
  ifzdal <= ifzdal;
  ifzdal <= ('L', 'L', 'W');
  ifzdal <= ifzdal;
  ifzdal <= ifzdal;
end syiomivtm;



-- Seed after: 722258468999594684,17219977430298397867
