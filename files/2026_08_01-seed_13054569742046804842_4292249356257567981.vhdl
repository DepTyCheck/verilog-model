-- Seed: 13054569742046804842,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (xfx : linkage std_logic; uezjnacp : out real_vector(0 downto 1));
end c;

architecture pfkigpnhj of c is
  
begin
  -- Single-driven assignments
  uezjnacp <= (others => 0.0);
end pfkigpnhj;

library ieee;
use ieee.std_logic_1164.all;

entity uaggecowp is
  port (ksjffktht : inout std_logic);
end uaggecowp;

architecture dmhyxxb of uaggecowp is
  signal vspvlgx : real_vector(0 downto 1);
begin
  nwhix : entity work.c
    port map (xfx => ksjffktht, uezjnacp => vspvlgx);
end dmhyxxb;

library ieee;
use ieee.std_logic_1164.all;

entity ri is
  port (ko : inout real; hjouk : linkage std_logic; qnsxtxccaj : inout std_logic);
end ri;

library ieee;
use ieee.std_logic_1164.all;

architecture agzdzew of ri is
  signal qxh : real_vector(0 downto 1);
  signal grc : real_vector(0 downto 1);
  signal qcczjtk : std_logic;
begin
  slnj : entity work.uaggecowp
    port map (ksjffktht => qnsxtxccaj);
  xrkzctjnn : entity work.uaggecowp
    port map (ksjffktht => qnsxtxccaj);
  pg : entity work.c
    port map (xfx => qcczjtk, uezjnacp => grc);
  at : entity work.c
    port map (xfx => qcczjtk, uezjnacp => qxh);
  
  -- Single-driven assignments
  ko <= 32.0414;
  
  -- Multi-driven assignments
  qcczjtk <= '1';
  qnsxtxccaj <= qnsxtxccaj;
  qnsxtxccaj <= '0';
  qcczjtk <= 'Z';
end agzdzew;



-- Seed after: 14137782981262456321,4292249356257567981
