-- Seed: 1705034636545087837,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity kfxss is
  port (unagho : linkage integer; bq : out std_logic);
end kfxss;



architecture urq of kfxss is
  
begin
  
end urq;

library ieee;
use ieee.std_logic_1164.all;

entity zpb is
  port (blh : buffer time; ne : buffer bit_vector(0 to 2); oqtplhnkc : buffer std_logic; lfcqnomt : linkage integer);
end zpb;

library ieee;
use ieee.std_logic_1164.all;

architecture txwlve of zpb is
  signal qtpmwmdfc : integer;
  signal kkpxcnzupk : std_logic;
  signal h : integer;
begin
  chpqhu : entity work.kfxss
    port map (unagho => h, bq => kkpxcnzupk);
  qtxmojp : entity work.kfxss
    port map (unagho => qtpmwmdfc, bq => oqtplhnkc);
  rjzkjlqgap : entity work.kfxss
    port map (unagho => lfcqnomt, bq => kkpxcnzupk);
end txwlve;



entity byvoqbdhk is
  port (er : in time; hql : in real; kpwbuewlz : buffer boolean_vector(2 to 4));
end byvoqbdhk;



architecture s of byvoqbdhk is
  
begin
  
end s;



entity zsj is
  port (fzcjrnh : inout time);
end zsj;

library ieee;
use ieee.std_logic_1164.all;

architecture spukezal of zsj is
  signal mivhhtjq : integer;
  signal przwrvrvy : std_logic;
  signal kig : bit_vector(0 to 2);
  signal ofeqit : time;
  signal dxmclrn : boolean_vector(2 to 4);
  signal kh : boolean_vector(2 to 4);
  signal c : real;
  signal yotqawaw : time;
begin
  qqgbwtwz : entity work.byvoqbdhk
    port map (er => yotqawaw, hql => c, kpwbuewlz => kh);
  orbubn : entity work.byvoqbdhk
    port map (er => fzcjrnh, hql => c, kpwbuewlz => dxmclrn);
  dws : entity work.zpb
    port map (blh => ofeqit, ne => kig, oqtplhnkc => przwrvrvy, lfcqnomt => mivhhtjq);
  vikvauqgv : entity work.kfxss
    port map (unagho => mivhhtjq, bq => przwrvrvy);
end spukezal;



-- Seed after: 12689437602188105580,11387579217500963635
