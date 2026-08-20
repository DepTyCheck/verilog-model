-- Seed: 15263853603024073899,499459191852795575

entity a is
  port (ydfjboz : inout time_vector(2 to 0));
end a;

architecture opqiyxpans of a is
  
begin
  -- Single-driven assignments
  ydfjboz <= ydfjboz;
end opqiyxpans;

library ieee;
use ieee.std_logic_1164.all;

entity osgvuc is
  port (ecnw : buffer std_logic);
end osgvuc;

architecture uodbtt of osgvuc is
  signal auyxvisg : time_vector(2 to 0);
begin
  xn : entity work.a
    port map (ydfjboz => auyxvisg);
  
  -- Multi-driven assignments
  ecnw <= '0';
  ecnw <= 'X';
  ecnw <= ecnw;
  ecnw <= '0';
end uodbtt;

entity lnddfbiqt is
  port (kc : in time);
end lnddfbiqt;

library ieee;
use ieee.std_logic_1164.all;

architecture fnqev of lnddfbiqt is
  signal zjwlmnpq : std_logic;
  signal zm : time_vector(2 to 0);
  signal cmbxk : time_vector(2 to 0);
begin
  m : entity work.a
    port map (ydfjboz => cmbxk);
  dwecuufz : entity work.a
    port map (ydfjboz => zm);
  isdeeriaua : entity work.osgvuc
    port map (ecnw => zjwlmnpq);
  rzmriycbb : entity work.osgvuc
    port map (ecnw => zjwlmnpq);
  
  -- Multi-driven assignments
  zjwlmnpq <= 'U';
  zjwlmnpq <= 'U';
end fnqev;



-- Seed after: 6568189458101561340,499459191852795575
