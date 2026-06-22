-- Seed: 10120608713721425215,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity afkj is
  port (xivydlghy : buffer boolean_vector(0 to 2); fuwswk : out std_logic_vector(2 downto 4); jhsx : buffer time);
end afkj;

architecture tvflxbvqst of afkj is
  
begin
  -- Single-driven assignments
  jhsx <= 0.43204 ms;
  
  -- Multi-driven assignments
  fuwswk <= (others => '0');
  fuwswk <= (others => '0');
  fuwswk <= "";
  fuwswk <= (others => '0');
end tvflxbvqst;

library ieee;
use ieee.std_logic_1164.all;

entity dvucdlc is
  port (tbfxckzefc : in std_logic_vector(1 to 3); ywsamjkf : linkage integer);
end dvucdlc;

library ieee;
use ieee.std_logic_1164.all;

architecture dr of dvucdlc is
  signal p : time;
  signal t : boolean_vector(0 to 2);
  signal gzc : time;
  signal ruolfsv : std_logic_vector(2 downto 4);
  signal rpbocfrow : boolean_vector(0 to 2);
  signal pejlk : time;
  signal biidlzao : std_logic_vector(2 downto 4);
  signal gl : boolean_vector(0 to 2);
begin
  tk : entity work.afkj
    port map (xivydlghy => gl, fuwswk => biidlzao, jhsx => pejlk);
  wjby : entity work.afkj
    port map (xivydlghy => rpbocfrow, fuwswk => ruolfsv, jhsx => gzc);
  wzcwucyq : entity work.afkj
    port map (xivydlghy => t, fuwswk => ruolfsv, jhsx => p);
  
  -- Multi-driven assignments
  biidlzao <= "";
end dr;



-- Seed after: 3432837708697965369,13479070923501788437
