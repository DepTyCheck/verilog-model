-- Seed: 9682855649165290734,4292249356257567981

entity q is
  port (ssl : in time; es : out integer_vector(3 downto 0); kypwlp : buffer character);
end q;

architecture u of q is
  
begin
  -- Single-driven assignments
  kypwlp <= 'e';
  es <= (8#2#, 33, 2#0_1_0_1_1#, 33113);
end u;

library ieee;
use ieee.std_logic_1164.all;

entity vzsu is
  port (utzazmrjt : out std_logic_vector(1 downto 4));
end vzsu;

architecture n of vzsu is
  signal tfg : character;
  signal nkt : integer_vector(3 downto 0);
  signal xa : character;
  signal x : integer_vector(3 downto 0);
  signal bditthiq : time;
  signal fhxyebo : character;
  signal fpskchflxi : integer_vector(3 downto 0);
  signal tqmfsjhqdl : time;
begin
  fsrdsw : entity work.q
    port map (ssl => tqmfsjhqdl, es => fpskchflxi, kypwlp => fhxyebo);
  opntqmun : entity work.q
    port map (ssl => bditthiq, es => x, kypwlp => xa);
  hieatco : entity work.q
    port map (ssl => bditthiq, es => nkt, kypwlp => tfg);
  
  -- Single-driven assignments
  tqmfsjhqdl <= 16#7A# ps;
  
  -- Multi-driven assignments
  utzazmrjt <= "";
end n;



-- Seed after: 12006417037380415695,4292249356257567981
