-- Seed: 13401505782609646628,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity rfg is
  port (rayr : buffer boolean_vector(3 to 3); p : in std_logic; oqfvjwb : linkage std_logic_vector(1 to 0); nb : in std_logic);
end rfg;

architecture abhudxy of rfg is
  
begin
  -- Single-driven assignments
  rayr <= (others => FALSE);
end abhudxy;

entity fvszzee is
  port (lnqckrrhd : out real);
end fvszzee;

architecture iqazejwsnv of fvszzee is
  
begin
  -- Single-driven assignments
  lnqckrrhd <= lnqckrrhd;
end iqazejwsnv;

entity extkzdvakt is
  port (qwxfaopy : buffer real);
end extkzdvakt;

library ieee;
use ieee.std_logic_1164.all;

architecture gbyhsa of extkzdvakt is
  signal mz : std_logic;
  signal vawyycxz : boolean_vector(3 to 3);
  signal ruzip : std_logic;
  signal xk : std_logic_vector(1 to 0);
  signal hxbsga : std_logic;
  signal awi : boolean_vector(3 to 3);
begin
  xpgm : entity work.fvszzee
    port map (lnqckrrhd => qwxfaopy);
  tvmnsnlzqf : entity work.rfg
    port map (rayr => awi, p => hxbsga, oqfvjwb => xk, nb => ruzip);
  pbnk : entity work.rfg
    port map (rayr => vawyycxz, p => hxbsga, oqfvjwb => xk, nb => mz);
end gbyhsa;



-- Seed after: 12589050601552261530,2230106469645304029
