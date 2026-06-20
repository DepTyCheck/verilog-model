-- Seed: 12050498249626916144,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity pginkpc is
  port (eukqa : out std_logic_vector(0 downto 1));
end pginkpc;

architecture frvxh of pginkpc is
  
begin
  -- Multi-driven assignments
  eukqa <= "";
  eukqa <= "";
end frvxh;

entity wvm is
  port (bg : in real; e : buffer time);
end wvm;

architecture ioyj of wvm is
  
begin
  
end ioyj;

entity gybuwktpkr is
  port (rdmyqzgi : buffer real);
end gybuwktpkr;

library ieee;
use ieee.std_logic_1164.all;

architecture fveum of gybuwktpkr is
  signal nxzd : time;
  signal tuqymv : std_logic_vector(0 downto 1);
begin
  wqwqh : entity work.pginkpc
    port map (eukqa => tuqymv);
  vfytz : entity work.wvm
    port map (bg => rdmyqzgi, e => nxzd);
  bal : entity work.pginkpc
    port map (eukqa => tuqymv);
  cxrgpcafo : entity work.pginkpc
    port map (eukqa => tuqymv);
  
  -- Single-driven assignments
  rdmyqzgi <= 2#0.1_0_0_1#;
  
  -- Multi-driven assignments
  tuqymv <= (others => '0');
  tuqymv <= "";
  tuqymv <= "";
end fveum;



-- Seed after: 5511491979743287532,3924983747739634027
