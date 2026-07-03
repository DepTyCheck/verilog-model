-- Seed: 3662179989667246182,2158184632809654795

library ieee;
use ieee.std_logic_1164.all;

entity snclxtjle is
  port (jxypqg : inout std_logic; uvyr : buffer bit);
end snclxtjle;

architecture ylaszor of snclxtjle is
  
begin
  -- Single-driven assignments
  uvyr <= uvyr;
  
  -- Multi-driven assignments
  jxypqg <= 'U';
end ylaszor;

entity yh is
  port (ifwdus : inout bit_vector(2 downto 0));
end yh;

library ieee;
use ieee.std_logic_1164.all;

architecture wxo of yh is
  signal rh : bit;
  signal bvtp : std_logic;
  signal x : bit;
  signal khay : bit;
  signal z : bit;
  signal xsztndi : std_logic;
begin
  tf : entity work.snclxtjle
    port map (jxypqg => xsztndi, uvyr => z);
  e : entity work.snclxtjle
    port map (jxypqg => xsztndi, uvyr => khay);
  xasgl : entity work.snclxtjle
    port map (jxypqg => xsztndi, uvyr => x);
  mtpmwnwwzn : entity work.snclxtjle
    port map (jxypqg => bvtp, uvyr => rh);
  
  -- Single-driven assignments
  ifwdus <= ('1', '1', '1');
end wxo;



-- Seed after: 12844482231637196521,2158184632809654795
