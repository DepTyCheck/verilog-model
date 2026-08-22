-- Seed: 13545017506338560816,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity gglv is
  port (irgawixjb : inout time; gvivt : inout time; vlewhqj : buffer std_logic);
end gglv;

architecture bobdzkyu of gglv is
  
begin
  -- Single-driven assignments
  gvivt <= irgawixjb;
  irgawixjb <= gvivt;
  
  -- Multi-driven assignments
  vlewhqj <= vlewhqj;
end bobdzkyu;

library ieee;
use ieee.std_logic_1164.all;

entity iedjcb is
  port (kplnvdjc : buffer std_logic_vector(4 downto 2); pncjtvroc : in real; hhbeqv : out bit; mnwagmr : linkage std_logic);
end iedjcb;

library ieee;
use ieee.std_logic_1164.all;

architecture eijmedo of iedjcb is
  signal apg : std_logic;
  signal iklsjk : time;
  signal o : time;
begin
  pbgbsvgdap : entity work.gglv
    port map (irgawixjb => o, gvivt => iklsjk, vlewhqj => apg);
  
  -- Single-driven assignments
  hhbeqv <= hhbeqv;
  
  -- Multi-driven assignments
  kplnvdjc <= "01Z";
  kplnvdjc <= ('W', '0', 'Z');
  kplnvdjc <= "WW-";
  kplnvdjc <= ('1', 'X', 'H');
end eijmedo;

library ieee;
use ieee.std_logic_1164.all;

entity hsjympu is
  port (t : in std_logic_vector(3 to 3); gpmdbvyi : inout integer);
end hsjympu;

library ieee;
use ieee.std_logic_1164.all;

architecture m of hsjympu is
  signal di : time;
  signal wr : time;
  signal po : std_logic;
  signal ljdra : bit;
  signal bsai : real;
  signal eazpqzxt : std_logic_vector(4 downto 2);
begin
  saipm : entity work.iedjcb
    port map (kplnvdjc => eazpqzxt, pncjtvroc => bsai, hhbeqv => ljdra, mnwagmr => po);
  p : entity work.gglv
    port map (irgawixjb => wr, gvivt => di, vlewhqj => po);
  
  -- Single-driven assignments
  gpmdbvyi <= 2#0#;
  bsai <= 2.24;
  
  -- Multi-driven assignments
  po <= 'H';
end m;



-- Seed after: 12655651293721179361,5805648483995786113
