-- Seed: 278526923181146212,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity zggtsw is
  port (rwtmiccd : in bit; iend : buffer std_logic_vector(4 downto 2));
end zggtsw;

architecture gpkly of zggtsw is
  
begin
  -- Multi-driven assignments
  iend <= iend;
end gpkly;

library ieee;
use ieee.std_logic_1164.all;

entity xhxo is
  port (jm : inout std_logic_vector(1 downto 1));
end xhxo;

library ieee;
use ieee.std_logic_1164.all;

architecture v of xhxo is
  signal cppjubpxst : std_logic_vector(4 downto 2);
  signal pzq : bit;
begin
  tqwlnoyx : entity work.zggtsw
    port map (rwtmiccd => pzq, iend => cppjubpxst);
  lhbpxpfoi : entity work.zggtsw
    port map (rwtmiccd => pzq, iend => cppjubpxst);
  kqpjqt : entity work.zggtsw
    port map (rwtmiccd => pzq, iend => cppjubpxst);
  
  -- Single-driven assignments
  pzq <= '0';
  
  -- Multi-driven assignments
  jm <= "Z";
  jm <= "L";
  jm <= (others => 'W');
  jm <= jm;
end v;



-- Seed after: 6413607809985171399,8927267689619684183
