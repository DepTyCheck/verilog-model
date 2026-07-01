-- Seed: 2082843756023551763,6882842853887419669

entity nfvem is
  port (rbwyglrlzf : buffer real);
end nfvem;

architecture dzaf of nfvem is
  
begin
  -- Single-driven assignments
  rbwyglrlzf <= 10.1_1_3;
end dzaf;

library ieee;
use ieee.std_logic_1164.all;

entity cf is
  port (vjlwglij : out integer; vxwkmwqq : buffer std_logic_vector(0 to 4));
end cf;

architecture wrxyp of cf is
  signal nre : real;
begin
  cfsfaek : entity work.nfvem
    port map (rbwyglrlzf => nre);
  
  -- Single-driven assignments
  vjlwglij <= 2#00110#;
  
  -- Multi-driven assignments
  vxwkmwqq <= "01XXU";
  vxwkmwqq <= "XWZUL";
  vxwkmwqq <= ('W', 'Z', '-', 'X', 'U');
end wrxyp;



-- Seed after: 11751640321812364074,6882842853887419669
