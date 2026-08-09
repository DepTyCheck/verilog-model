-- Seed: 4402452946588930269,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity tkdorgj is
  port (fzuq : in bit; xd : inout std_logic);
end tkdorgj;

architecture ohwtkrc of tkdorgj is
  
begin
  -- Multi-driven assignments
  xd <= 'U';
  xd <= '-';
  xd <= xd;
  xd <= xd;
end ohwtkrc;

library ieee;
use ieee.std_logic_1164.all;

entity aexer is
  port (gtaq : inout std_logic_vector(1 to 3); csxyfpuq : in time);
end aexer;

library ieee;
use ieee.std_logic_1164.all;

architecture fuff of aexer is
  signal xbjqv : std_logic;
  signal gifyyzk : std_logic;
  signal bvwhs : bit;
  signal wm : std_logic;
  signal pjvkpeyfxy : bit;
begin
  dlirqhg : entity work.tkdorgj
    port map (fzuq => pjvkpeyfxy, xd => wm);
  ghwuym : entity work.tkdorgj
    port map (fzuq => bvwhs, xd => gifyyzk);
  lodmpa : entity work.tkdorgj
    port map (fzuq => pjvkpeyfxy, xd => xbjqv);
  
  -- Single-driven assignments
  pjvkpeyfxy <= '0';
  bvwhs <= '0';
  
  -- Multi-driven assignments
  gifyyzk <= 'W';
end fuff;



-- Seed after: 3687378944989054093,10871023049702252113
