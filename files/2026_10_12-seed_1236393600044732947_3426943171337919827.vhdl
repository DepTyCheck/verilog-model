-- Seed: 1236393600044732947,3426943171337919827

entity yqhntxd is
  port (vitlqtro : out boolean_vector(1 downto 3));
end yqhntxd;

architecture hvgfytds of yqhntxd is
  
begin
  
end hvgfytds;

library ieee;
use ieee.std_logic_1164.all;

entity cpqhgil is
  port (bix : buffer real; xtzuao : buffer real; o : out std_logic);
end cpqhgil;

architecture cpotsm of cpqhgil is
  signal nnjpcudoe : boolean_vector(1 downto 3);
  signal f : boolean_vector(1 downto 3);
  signal rcc : boolean_vector(1 downto 3);
  signal zhouq : boolean_vector(1 downto 3);
begin
  pbawuuct : entity work.yqhntxd
    port map (vitlqtro => zhouq);
  fxpo : entity work.yqhntxd
    port map (vitlqtro => rcc);
  vwbdbc : entity work.yqhntxd
    port map (vitlqtro => f);
  nqrw : entity work.yqhntxd
    port map (vitlqtro => nnjpcudoe);
  
  -- Single-driven assignments
  xtzuao <= 16#A9.665D#;
  bix <= xtzuao;
  
  -- Multi-driven assignments
  o <= o;
  o <= o;
  o <= '-';
  o <= 'U';
end cpotsm;



-- Seed after: 11538910007394836579,3426943171337919827
