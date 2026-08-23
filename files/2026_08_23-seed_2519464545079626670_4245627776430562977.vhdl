-- Seed: 2519464545079626670,4245627776430562977

entity ssrbrmslcl is
  port (shk : buffer time; jzgovyvyro : in real; malzdzvte : linkage integer; hdytr : linkage real);
end ssrbrmslcl;

architecture y of ssrbrmslcl is
  
begin
  -- Single-driven assignments
  shk <= 8#5_4# us;
end y;

entity brgch is
  port (hfjqdurqi : buffer bit_vector(3 to 2));
end brgch;

architecture fbcyzo of brgch is
  signal jzvsfisalr : integer;
  signal isrzok : real;
  signal hvhtfsalx : time;
  signal ndinfyy : integer;
  signal rf : real;
  signal loy : time;
begin
  jvz : entity work.ssrbrmslcl
    port map (shk => loy, jzgovyvyro => rf, malzdzvte => ndinfyy, hdytr => rf);
  vqmxgnc : entity work.ssrbrmslcl
    port map (shk => hvhtfsalx, jzgovyvyro => isrzok, malzdzvte => jzvsfisalr, hdytr => isrzok);
  
  -- Single-driven assignments
  hfjqdurqi <= hfjqdurqi;
end fbcyzo;

library ieee;
use ieee.std_logic_1164.all;

entity vrn is
  port (viff : inout std_logic; xjpyqqqx : inout time);
end vrn;

architecture wzjpjrg of vrn is
  signal rklcmutvp : integer;
  signal umzal : real;
begin
  auibwv : entity work.ssrbrmslcl
    port map (shk => xjpyqqqx, jzgovyvyro => umzal, malzdzvte => rklcmutvp, hdytr => umzal);
  
  -- Multi-driven assignments
  viff <= 'X';
end wzjpjrg;



-- Seed after: 67221953719237509,4245627776430562977
