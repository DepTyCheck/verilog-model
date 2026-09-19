-- Seed: 12580443243162081139,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity su is
  port (ao : out std_logic; jbr : linkage std_logic_vector(4 to 0));
end su;

architecture nb of su is
  
begin
  -- Multi-driven assignments
  ao <= 'X';
  ao <= '-';
end nb;

entity tgwu is
  port (mzvtd : in time);
end tgwu;

library ieee;
use ieee.std_logic_1164.all;

architecture mbn of tgwu is
  signal xlpz : std_logic_vector(4 to 0);
  signal puq : std_logic;
  signal hrtgb : std_logic_vector(4 to 0);
  signal ldka : std_logic;
  signal fxmqedg : std_logic_vector(4 to 0);
  signal wsarqu : std_logic;
begin
  bjqu : entity work.su
    port map (ao => wsarqu, jbr => fxmqedg);
  ymimv : entity work.su
    port map (ao => ldka, jbr => hrtgb);
  mqldock : entity work.su
    port map (ao => puq, jbr => xlpz);
  
  -- Multi-driven assignments
  wsarqu <= wsarqu;
  xlpz <= fxmqedg;
  ldka <= 'X';
end mbn;



-- Seed after: 6953769321503036931,14141408946471626091
