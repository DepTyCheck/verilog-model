-- Seed: 3047982571233603470,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity dpyxcnpa is
  port (ayma : buffer real; ria : linkage time_vector(4 to 1); zhvepx : buffer std_logic_vector(2 to 3); gbisejwgc : in std_logic);
end dpyxcnpa;

architecture oeoyistlxg of dpyxcnpa is
  
begin
  -- Single-driven assignments
  ayma <= 16#33.A#;
  
  -- Multi-driven assignments
  zhvepx <= ('X', 'U');
  zhvepx <= ('W', 'L');
end oeoyistlxg;

entity lhvlqrwsl is
  port (ryqdhvlwz : buffer time);
end lhvlqrwsl;

library ieee;
use ieee.std_logic_1164.all;

architecture xyji of lhvlqrwsl is
  signal occmknuf : std_logic;
  signal vjjppgbxx : std_logic_vector(2 to 3);
  signal sexlyitmsr : time_vector(4 to 1);
  signal prf : real;
begin
  kgzy : entity work.dpyxcnpa
    port map (ayma => prf, ria => sexlyitmsr, zhvepx => vjjppgbxx, gbisejwgc => occmknuf);
  
  -- Single-driven assignments
  ryqdhvlwz <= 2#00.00# fs;
  
  -- Multi-driven assignments
  vjjppgbxx <= vjjppgbxx;
  vjjppgbxx <= "XU";
  vjjppgbxx <= vjjppgbxx;
end xyji;



-- Seed after: 2917857484324319260,511364357853360275
