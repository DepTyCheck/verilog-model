-- Seed: 7994655573611637388,4122021602305298647

entity e is
  port (nbxb : in boolean_vector(4 to 3); fjqdvoyyd : out time; y : in integer);
end e;

architecture opzbwoi of e is
  
begin
  
end opzbwoi;

entity epvqg is
  port (ci : out time_vector(0 downto 4));
end epvqg;

architecture uqdcan of epvqg is
  signal eblftgm : integer;
  signal uuxdztyas : time;
  signal mhivzzqgjl : boolean_vector(4 to 3);
begin
  ajxypq : entity work.e
    port map (nbxb => mhivzzqgjl, fjqdvoyyd => uuxdztyas, y => eblftgm);
  
  -- Single-driven assignments
  mhivzzqgjl <= mhivzzqgjl;
end uqdcan;

library ieee;
use ieee.std_logic_1164.all;

entity trvow is
  port (rj : linkage integer; zbwlwe : buffer integer; wgouo : inout time; kdbmwzy : inout std_logic_vector(3 to 0));
end trvow;

architecture jrvqc of trvow is
  signal ah : time;
  signal xipyttkj : boolean_vector(4 to 3);
  signal voihydgil : integer;
  signal cxonwuwknj : boolean_vector(4 to 3);
begin
  pt : entity work.e
    port map (nbxb => cxonwuwknj, fjqdvoyyd => wgouo, y => voihydgil);
  hlwy : entity work.e
    port map (nbxb => xipyttkj, fjqdvoyyd => ah, y => zbwlwe);
  
  -- Single-driven assignments
  cxonwuwknj <= cxonwuwknj;
end jrvqc;

entity snat is
  port (jmu : buffer integer_vector(2 downto 4); zp : in time; mczevacmr : in real);
end snat;

library ieee;
use ieee.std_logic_1164.all;

architecture g of snat is
  signal forarn : time;
  signal mtfphtbvwm : boolean_vector(4 to 3);
  signal dflnrlp : std_logic_vector(3 to 0);
  signal prztjnzxm : time;
  signal jdlkbcj : integer;
  signal r : integer;
  signal fmjzpolbwa : time;
  signal kmslfxo : boolean_vector(4 to 3);
begin
  yqopry : entity work.e
    port map (nbxb => kmslfxo, fjqdvoyyd => fmjzpolbwa, y => r);
  mdyevbnhza : entity work.trvow
    port map (rj => jdlkbcj, zbwlwe => r, wgouo => prztjnzxm, kdbmwzy => dflnrlp);
  uqzlkm : entity work.e
    port map (nbxb => mtfphtbvwm, fjqdvoyyd => forarn, y => r);
  
  -- Single-driven assignments
  jmu <= (others => 0);
  
  -- Multi-driven assignments
  dflnrlp <= dflnrlp;
  dflnrlp <= dflnrlp;
end g;



-- Seed after: 3087229343015082763,4122021602305298647
