-- Seed: 10509093680805672592,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity sjcu is
  port (tnrmkodqy : inout time; fxom : out std_logic_vector(4 downto 3); bbwkaileqp : linkage std_logic; vqab : in real);
end sjcu;

architecture dqynnoz of sjcu is
  
begin
  -- Multi-driven assignments
  fxom <= ('L', 'W');
end dqynnoz;

entity breodbeyxv is
  port (y : linkage real);
end breodbeyxv;

library ieee;
use ieee.std_logic_1164.all;

architecture dyg of breodbeyxv is
  signal kxlusvqjn : real;
  signal oakswyqnzn : std_logic_vector(4 downto 3);
  signal g : time;
  signal iefkd : real;
  signal vvrogyia : std_logic;
  signal ofyptdvuz : std_logic_vector(4 downto 3);
  signal fv : time;
begin
  uyueaggmcq : entity work.sjcu
    port map (tnrmkodqy => fv, fxom => ofyptdvuz, bbwkaileqp => vvrogyia, vqab => iefkd);
  omnhrxrrm : entity work.sjcu
    port map (tnrmkodqy => g, fxom => oakswyqnzn, bbwkaileqp => vvrogyia, vqab => kxlusvqjn);
end dyg;

library ieee;
use ieee.std_logic_1164.all;

entity xnxgctqny is
  port (gos : inout std_logic_vector(4 to 2));
end xnxgctqny;

architecture yrvosujkmy of xnxgctqny is
  signal vzyke : real;
begin
  nltulo : entity work.breodbeyxv
    port map (y => vzyke);
end yrvosujkmy;



-- Seed after: 15806597662626399854,17219977430298397867
