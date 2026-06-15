-- Seed: 10248088517496113991,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (ed : in std_logic; mcfapfjv : out real);
end p;

architecture gcnndhumjk of p is
  
begin
  
end gcnndhumjk;

library ieee;
use ieee.std_logic_1164.all;

entity rglzlxl is
  port (j : in std_logic_vector(3 to 1); xoecpvkeqd : buffer character; gydim : in string(4 to 3); xcmxr : out integer);
end rglzlxl;

library ieee;
use ieee.std_logic_1164.all;

architecture aaxiiv of rglzlxl is
  signal hpxgbx : real;
  signal kgvkvyi : real;
  signal zbnlurwnw : std_logic;
begin
  oyorkcfywh : entity work.p
    port map (ed => zbnlurwnw, mcfapfjv => kgvkvyi);
  gqhmsw : entity work.p
    port map (ed => zbnlurwnw, mcfapfjv => hpxgbx);
  
  -- Single-driven assignments
  xoecpvkeqd <= 'n';
  xcmxr <= 0_0_0;
  
  -- Multi-driven assignments
  zbnlurwnw <= 'W';
  zbnlurwnw <= 'Z';
end aaxiiv;

entity iywgxz is
  port (jiq : linkage integer);
end iywgxz;

library ieee;
use ieee.std_logic_1164.all;

architecture dpi of iywgxz is
  signal fcvgx : integer;
  signal ihcrprrj : string(4 to 3);
  signal cuhao : character;
  signal jrbp : std_logic_vector(3 to 1);
begin
  aoufq : entity work.rglzlxl
    port map (j => jrbp, xoecpvkeqd => cuhao, gydim => ihcrprrj, xcmxr => fcvgx);
  
  -- Single-driven assignments
  ihcrprrj <= (others => ' ');
  
  -- Multi-driven assignments
  jrbp <= "";
end dpi;



-- Seed after: 10051692278674400869,15300320181035395489
