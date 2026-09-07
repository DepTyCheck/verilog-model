-- Seed: 1421674872856206441,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity audcnb is
  port (kxtb : in std_logic_vector(2 to 0); dtncazzgo : linkage bit_vector(2 to 0));
end audcnb;

architecture ahhhed of audcnb is
  
begin
  
end ahhhed;

entity bjjiiejy is
  port (ozgbdpm : out real; dykz : linkage time; atnbxpgmc : in real);
end bjjiiejy;

library ieee;
use ieee.std_logic_1164.all;

architecture fx of bjjiiejy is
  signal mecjh : bit_vector(2 to 0);
  signal wcwsoiyx : std_logic_vector(2 to 0);
  signal ttkm : bit_vector(2 to 0);
  signal fudw : std_logic_vector(2 to 0);
  signal meua : bit_vector(2 to 0);
  signal yezl : std_logic_vector(2 to 0);
begin
  dkltdip : entity work.audcnb
    port map (kxtb => yezl, dtncazzgo => meua);
  ucoapfgs : entity work.audcnb
    port map (kxtb => fudw, dtncazzgo => ttkm);
  ohj : entity work.audcnb
    port map (kxtb => wcwsoiyx, dtncazzgo => mecjh);
  
  -- Single-driven assignments
  ozgbdpm <= atnbxpgmc;
  
  -- Multi-driven assignments
  yezl <= (others => '0');
  fudw <= (others => '0');
end fx;

library ieee;
use ieee.std_logic_1164.all;

entity xz is
  port (mggsr : inout boolean; boviginotg : inout std_logic; n : in integer; grn : inout integer);
end xz;

library ieee;
use ieee.std_logic_1164.all;

architecture wjvcmjryhs of xz is
  signal fedqlw : bit_vector(2 to 0);
  signal qmaysljmy : std_logic_vector(2 to 0);
begin
  kymf : entity work.audcnb
    port map (kxtb => qmaysljmy, dtncazzgo => fedqlw);
  
  -- Single-driven assignments
  grn <= grn;
  mggsr <= FALSE;
  
  -- Multi-driven assignments
  boviginotg <= '0';
end wjvcmjryhs;

library ieee;
use ieee.std_logic_1164.all;

entity ctpybchka is
  port (vtut : linkage std_logic_vector(3 to 1); uv : inout std_logic_vector(3 to 4); mwlifegif : buffer real_vector(1 to 1));
end ctpybchka;

library ieee;
use ieee.std_logic_1164.all;

architecture wa of ctpybchka is
  signal xss : bit_vector(2 to 0);
  signal mcuai : std_logic_vector(2 to 0);
begin
  upaqmngie : entity work.audcnb
    port map (kxtb => mcuai, dtncazzgo => xss);
  
  -- Single-driven assignments
  mwlifegif <= mwlifegif;
  
  -- Multi-driven assignments
  uv <= uv;
  uv <= uv;
  uv <= "0Z";
  uv <= ('X', 'U');
end wa;



-- Seed after: 17656389443633121033,12269339630485015285
