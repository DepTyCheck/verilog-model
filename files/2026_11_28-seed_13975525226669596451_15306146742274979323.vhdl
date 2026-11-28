-- Seed: 13975525226669596451,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity alqf is
  port (nrfmvayqaf : buffer real; cllcqhikqj : out std_logic_vector(1 to 3); hxddhdi : inout time_vector(2 to 4); e : linkage real);
end alqf;

architecture oecoho of alqf is
  
begin
  -- Single-driven assignments
  hxddhdi <= hxddhdi;
  nrfmvayqaf <= 1.042;
  
  -- Multi-driven assignments
  cllcqhikqj <= ('H', 'Z', '1');
end oecoho;

library ieee;
use ieee.std_logic_1164.all;

entity ieznrifu is
  port (tlvshosu : out integer; easntgx : inout real; gbri : linkage std_logic_vector(1 downto 0));
end ieznrifu;

library ieee;
use ieee.std_logic_1164.all;

architecture ysji of ieznrifu is
  signal wfueule : time_vector(2 to 4);
  signal q : real;
  signal okwi : real;
  signal bepkzps : time_vector(2 to 4);
  signal mlfdlvm : std_logic_vector(1 to 3);
  signal llxencns : real;
begin
  zkbvb : entity work.alqf
    port map (nrfmvayqaf => llxencns, cllcqhikqj => mlfdlvm, hxddhdi => bepkzps, e => okwi);
  kah : entity work.alqf
    port map (nrfmvayqaf => q, cllcqhikqj => mlfdlvm, hxddhdi => wfueule, e => easntgx);
  
  -- Multi-driven assignments
  mlfdlvm <= "-L0";
end ysji;



-- Seed after: 2205814821371681958,15306146742274979323
