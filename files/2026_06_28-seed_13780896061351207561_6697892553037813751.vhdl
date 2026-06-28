-- Seed: 13780896061351207561,6697892553037813751

entity l is
  port (gwdbu : buffer bit_vector(2 downto 3));
end l;

architecture pbea of l is
  
begin
  -- Single-driven assignments
  gwdbu <= (others => '0');
end pbea;

library ieee;
use ieee.std_logic_1164.all;

entity qd is
  port (sr : linkage boolean; hlg : inout std_logic_vector(0 to 3); naefzknii : inout bit_vector(1 downto 1));
end qd;

architecture dna of qd is
  
begin
  -- Multi-driven assignments
  hlg <= "U00X";
  hlg <= ('X', 'U', 'Z', 'Z');
end dna;

entity hu is
  port (xemcacygr : buffer string(3 to 1));
end hu;

library ieee;
use ieee.std_logic_1164.all;

architecture v of hu is
  signal mwhifjk : bit_vector(1 downto 1);
  signal hpjqfuh : std_logic_vector(0 to 3);
  signal ji : boolean;
begin
  yrlxm : entity work.qd
    port map (sr => ji, hlg => hpjqfuh, naefzknii => mwhifjk);
  
  -- Single-driven assignments
  xemcacygr <= (others => ' ');
  
  -- Multi-driven assignments
  hpjqfuh <= ('H', 'Z', 'Z', '-');
  hpjqfuh <= ('H', 'Z', '1', '0');
  hpjqfuh <= ('U', 'Z', '1', '0');
  hpjqfuh <= "0U10";
end v;

entity s is
  port (gqa : inout boolean; fgpuqeks : inout time; pa : inout real; oo : out time);
end s;

architecture caggb of s is
  signal tce : bit_vector(2 downto 3);
begin
  xx : entity work.l
    port map (gwdbu => tce);
end caggb;



-- Seed after: 16594713177500919888,6697892553037813751
