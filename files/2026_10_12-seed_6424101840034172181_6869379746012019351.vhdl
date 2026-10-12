-- Seed: 6424101840034172181,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (qhdaci : inout std_logic_vector(2 downto 1));
end j;

architecture ly of j is
  
begin
  -- Multi-driven assignments
  qhdaci <= ('1', '1');
end ly;

library ieee;
use ieee.std_logic_1164.all;

entity apx is
  port ( vqqllnl : inout time
  ; ttanwl : buffer time_vector(1 downto 0)
  ; oeamsc : linkage std_logic_vector(3 downto 3)
  ; bcwo : out std_logic_vector(0 downto 2)
  );
end apx;

library ieee;
use ieee.std_logic_1164.all;

architecture ztofjsmqww of apx is
  signal kn : std_logic_vector(2 downto 1);
  signal g : std_logic_vector(2 downto 1);
  signal bca : std_logic_vector(2 downto 1);
begin
  ni : entity work.j
    port map (qhdaci => bca);
  rkdqpps : entity work.j
    port map (qhdaci => g);
  ojume : entity work.j
    port map (qhdaci => g);
  sztgvulhgv : entity work.j
    port map (qhdaci => kn);
  
  -- Multi-driven assignments
  g <= "ZZ";
  kn <= bca;
end ztofjsmqww;



-- Seed after: 9807170178409607411,6869379746012019351
