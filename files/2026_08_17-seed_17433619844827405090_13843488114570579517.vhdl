-- Seed: 17433619844827405090,13843488114570579517

entity ckmwp is
  port (szjykt : inout real; wglsbfkm : in real);
end ckmwp;

architecture c of ckmwp is
  
begin
  -- Single-driven assignments
  szjykt <= 8#4464.770#;
end c;

library ieee;
use ieee.std_logic_1164.all;

entity housd is
  port (gjkzdafwjo : buffer std_logic);
end housd;

architecture cgsx of housd is
  signal nix : real;
  signal kyv : real;
  signal klkz : real;
  signal npmu : real;
  signal tryfxb : real;
begin
  hprst : entity work.ckmwp
    port map (szjykt => tryfxb, wglsbfkm => npmu);
  yg : entity work.ckmwp
    port map (szjykt => klkz, wglsbfkm => tryfxb);
  kuhsrrjba : entity work.ckmwp
    port map (szjykt => npmu, wglsbfkm => tryfxb);
  nffqeci : entity work.ckmwp
    port map (szjykt => kyv, wglsbfkm => nix);
  
  -- Single-driven assignments
  nix <= 2#11.0#;
  
  -- Multi-driven assignments
  gjkzdafwjo <= gjkzdafwjo;
end cgsx;

library ieee;
use ieee.std_logic_1164.all;

entity qz is
  port (fxigovc : out std_logic; filljwo : out integer_vector(3 downto 0); z : linkage integer; ifbr : out std_logic_vector(2 downto 4));
end qz;

library ieee;
use ieee.std_logic_1164.all;

architecture zfsrgrbkbd of qz is
  signal rsyxgpio : real;
  signal cfswwbiie : real;
  signal bjufdzpp : real;
  signal xviqhygzc : std_logic;
  signal vqnirw : std_logic;
begin
  tvprprgf : entity work.housd
    port map (gjkzdafwjo => vqnirw);
  a : entity work.housd
    port map (gjkzdafwjo => xviqhygzc);
  v : entity work.ckmwp
    port map (szjykt => bjufdzpp, wglsbfkm => cfswwbiie);
  rzcpoyf : entity work.ckmwp
    port map (szjykt => rsyxgpio, wglsbfkm => bjufdzpp);
  
  -- Single-driven assignments
  filljwo <= filljwo;
  cfswwbiie <= 8#0_1_1_2_6.64#;
  
  -- Multi-driven assignments
  xviqhygzc <= '-';
end zfsrgrbkbd;



-- Seed after: 9129569751747401590,13843488114570579517
