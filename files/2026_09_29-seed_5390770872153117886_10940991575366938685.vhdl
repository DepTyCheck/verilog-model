-- Seed: 5390770872153117886,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity ejpufukin is
  port (bvscmvb : in std_logic_vector(3 to 2); fbar : buffer boolean_vector(0 to 3); ngy : in string(3 downto 3));
end ejpufukin;

architecture ncsfyynwr of ejpufukin is
  
begin
  -- Single-driven assignments
  fbar <= (FALSE, FALSE, TRUE, FALSE);
end ncsfyynwr;

library ieee;
use ieee.std_logic_1164.all;

entity irllazd is
  port (aezseohtop : linkage std_logic);
end irllazd;

library ieee;
use ieee.std_logic_1164.all;

architecture wjwcep of irllazd is
  signal qxhr : boolean_vector(0 to 3);
  signal ggzo : string(3 downto 3);
  signal wi : boolean_vector(0 to 3);
  signal xyirfmyw : std_logic_vector(3 to 2);
begin
  ahatc : entity work.ejpufukin
    port map (bvscmvb => xyirfmyw, fbar => wi, ngy => ggzo);
  cmov : entity work.ejpufukin
    port map (bvscmvb => xyirfmyw, fbar => qxhr, ngy => ggzo);
  
  -- Single-driven assignments
  ggzo <= ggzo;
  
  -- Multi-driven assignments
  xyirfmyw <= xyirfmyw;
end wjwcep;



-- Seed after: 14901701198331579282,10940991575366938685
