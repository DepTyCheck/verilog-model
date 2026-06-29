-- Seed: 1676408719687992584,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity uggfdot is
  port (rfmf : inout std_logic; rtxvvwkmpr : buffer std_logic_vector(3 to 3); xc : out time);
end uggfdot;

architecture xyadtdus of uggfdot is
  
begin
  -- Single-driven assignments
  xc <= 2 sec;
  
  -- Multi-driven assignments
  rfmf <= '0';
  rfmf <= 'H';
end xyadtdus;

library ieee;
use ieee.std_logic_1164.all;

entity zxpiltitb is
  port (p : buffer boolean; gl : in std_logic_vector(3 to 1); kqbqcxspbp : inout real);
end zxpiltitb;

library ieee;
use ieee.std_logic_1164.all;

architecture zconrxma of zxpiltitb is
  signal d : time;
  signal esp : std_logic;
  signal w : time;
  signal wmzcw : std_logic_vector(3 to 3);
  signal m : std_logic;
begin
  khl : entity work.uggfdot
    port map (rfmf => m, rtxvvwkmpr => wmzcw, xc => w);
  qf : entity work.uggfdot
    port map (rfmf => esp, rtxvvwkmpr => wmzcw, xc => d);
  
  -- Single-driven assignments
  kqbqcxspbp <= 2#01001.1_1#;
  p <= TRUE;
  
  -- Multi-driven assignments
  wmzcw <= (others => 'X');
  wmzcw <= "H";
  m <= '0';
end zconrxma;



-- Seed after: 16474850411392275175,17047277710231705797
