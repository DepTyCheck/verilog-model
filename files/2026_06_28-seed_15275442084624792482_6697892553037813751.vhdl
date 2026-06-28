-- Seed: 15275442084624792482,6697892553037813751

entity gsdyzm is
  port (xxpsg : buffer boolean_vector(0 to 2));
end gsdyzm;

architecture dwfgmznd of gsdyzm is
  
begin
  -- Single-driven assignments
  xxpsg <= (FALSE, FALSE, TRUE);
end dwfgmznd;

library ieee;
use ieee.std_logic_1164.all;

entity mplgyoki is
  port (aevitsoi : inout character; mfndai : in std_logic_vector(0 downto 4); epzovmneaj : in bit_vector(4 to 4));
end mplgyoki;

architecture ma of mplgyoki is
  signal wpmxgr : boolean_vector(0 to 2);
begin
  tsarj : entity work.gsdyzm
    port map (xxpsg => wpmxgr);
  
  -- Single-driven assignments
  aevitsoi <= 'h';
end ma;



-- Seed after: 10919436393927517077,6697892553037813751
