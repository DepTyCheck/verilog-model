-- Seed: 4817691976239029448,13857275728440271305

entity ewmkkmoyz is
  port (xki : inout bit_vector(0 to 3));
end ewmkkmoyz;

architecture ig of ewmkkmoyz is
  
begin
  -- Single-driven assignments
  xki <= xki;
end ig;

library ieee;
use ieee.std_logic_1164.all;

entity yytbpggzn is
  port (gqwmzcyv : out std_logic; evvr : in std_logic_vector(2 downto 4));
end yytbpggzn;

architecture voqldqnr of yytbpggzn is
  signal xlhez : bit_vector(0 to 3);
  signal bqp : bit_vector(0 to 3);
  signal lho : bit_vector(0 to 3);
begin
  wzey : entity work.ewmkkmoyz
    port map (xki => lho);
  hsmlagj : entity work.ewmkkmoyz
    port map (xki => bqp);
  qkihfzzmv : entity work.ewmkkmoyz
    port map (xki => xlhez);
  
  -- Multi-driven assignments
  gqwmzcyv <= 'H';
end voqldqnr;



-- Seed after: 3726942254728867280,13857275728440271305
