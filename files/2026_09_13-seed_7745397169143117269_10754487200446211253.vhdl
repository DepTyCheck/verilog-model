-- Seed: 7745397169143117269,10754487200446211253

entity muxw is
  port (s : inout integer; xpv : out real; ervn : inout real);
end muxw;

architecture mf of muxw is
  
begin
  -- Single-driven assignments
  ervn <= 2#11.000#;
  s <= s;
  xpv <= ervn;
end mf;

library ieee;
use ieee.std_logic_1164.all;

entity sglja is
  port (qojyjcvwzr : inout std_logic; dpurnspbzl : buffer std_logic; r : in severity_level);
end sglja;

architecture xae of sglja is
  signal uxfv : real;
  signal jjngfo : real;
  signal gal : integer;
  signal qzcmwqhsh : real;
  signal ntbht : real;
  signal fsuawllvb : integer;
begin
  pfi : entity work.muxw
    port map (s => fsuawllvb, xpv => ntbht, ervn => qzcmwqhsh);
  hxbtpjux : entity work.muxw
    port map (s => gal, xpv => jjngfo, ervn => uxfv);
end xae;

entity sowuepo is
  port (qebeaf : out real);
end sowuepo;

architecture ggbogbgxpn of sowuepo is
  signal yfundd : real;
  signal l : integer;
  signal ex : real;
  signal ydkebub : real;
  signal mhwds : integer;
begin
  mppby : entity work.muxw
    port map (s => mhwds, xpv => ydkebub, ervn => ex);
  u : entity work.muxw
    port map (s => l, xpv => yfundd, ervn => qebeaf);
end ggbogbgxpn;



-- Seed after: 531829683546300455,10754487200446211253
