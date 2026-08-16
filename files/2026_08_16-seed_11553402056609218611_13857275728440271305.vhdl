-- Seed: 11553402056609218611,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (lmexeo : buffer std_logic_vector(1 downto 2); wdzbg : out std_logic);
end b;

architecture g of b is
  
begin
  -- Multi-driven assignments
  wdzbg <= 'Z';
  wdzbg <= wdzbg;
  wdzbg <= wdzbg;
end g;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (igtml : buffer severity_level; jh : linkage std_logic);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture zwlcff of p is
  signal xhsqaeeyy : std_logic;
  signal fyerbozj : std_logic_vector(1 downto 2);
begin
  hdgifxsnbw : entity work.b
    port map (lmexeo => fyerbozj, wdzbg => xhsqaeeyy);
  svd : entity work.b
    port map (lmexeo => fyerbozj, wdzbg => xhsqaeeyy);
  wg : entity work.b
    port map (lmexeo => fyerbozj, wdzbg => xhsqaeeyy);
  
  -- Single-driven assignments
  igtml <= ERROR;
end zwlcff;



-- Seed after: 5898439339228008438,13857275728440271305
