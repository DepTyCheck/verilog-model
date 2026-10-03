-- Seed: 4913728040890327973,6140041381800297705

entity ukdari is
  port (gyk : out time);
end ukdari;

architecture rcnolg of ukdari is
  
begin
  -- Single-driven assignments
  gyk <= gyk;
end rcnolg;

entity lvilwcw is
  port (jj : inout time; lhnjeurarg : out time; gqsdy : linkage integer);
end lvilwcw;

architecture megkyxpgjn of lvilwcw is
  signal qwvalyhxg : time;
  signal c : time;
begin
  pkmkdabuk : entity work.ukdari
    port map (gyk => lhnjeurarg);
  bfixl : entity work.ukdari
    port map (gyk => jj);
  zfo : entity work.ukdari
    port map (gyk => c);
  qoygxtnau : entity work.ukdari
    port map (gyk => qwvalyhxg);
end megkyxpgjn;

library ieee;
use ieee.std_logic_1164.all;

entity tbya is
  port (ueb : buffer integer_vector(2 to 2); rzzff : out real_vector(0 downto 2); leeuo : in std_logic_vector(0 to 1));
end tbya;

architecture lwrvxstep of tbya is
  signal nsbvpyaru : time;
  signal xm : integer;
  signal gtgxvbpfj : time;
  signal iswpu : time;
  signal craiykms : time;
begin
  vgrurwu : entity work.ukdari
    port map (gyk => craiykms);
  sg : entity work.lvilwcw
    port map (jj => iswpu, lhnjeurarg => gtgxvbpfj, gqsdy => xm);
  ll : entity work.ukdari
    port map (gyk => nsbvpyaru);
  
  -- Single-driven assignments
  rzzff <= rzzff;
  ueb <= (others => 2#001#);
end lwrvxstep;



-- Seed after: 11060332747618072973,6140041381800297705
