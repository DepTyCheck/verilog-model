-- Seed: 991800501358690767,6329330932550885447



entity v is
  port (mbare : inout integer);
end v;



architecture nofv of v is
  
begin
  
end nofv;

library ieee;
use ieee.std_logic_1164.all;

entity asyv is
  port (eicvrub : in std_logic_vector(4 downto 2); cpgseyeyjf : in time);
end asyv;



architecture iic of asyv is
  signal n : integer;
  signal cjdjcz : integer;
  signal iumfy : integer;
  signal juxovbpc : integer;
begin
  scvvxupora : entity work.v
    port map (mbare => juxovbpc);
  cqxtoxrgv : entity work.v
    port map (mbare => iumfy);
  nnjvuuuhmk : entity work.v
    port map (mbare => cjdjcz);
  ohmwefb : entity work.v
    port map (mbare => n);
end iic;

library ieee;
use ieee.std_logic_1164.all;

entity zsmlmnrikz is
  port (pa : in integer; fzq : out std_logic_vector(2 to 3));
end zsmlmnrikz;



architecture vlczwbcke of zsmlmnrikz is
  signal xfme : integer;
begin
  tnx : entity work.v
    port map (mbare => xfme);
end vlczwbcke;

library ieee;
use ieee.std_logic_1164.all;

entity nmiesycy is
  port (khwc : in real; loguv : out std_logic; jwshyl : in time);
end nmiesycy;

library ieee;
use ieee.std_logic_1164.all;

architecture w of nmiesycy is
  signal zn : integer;
  signal clra : std_logic_vector(2 to 3);
  signal lv : integer;
begin
  aeqzfpc : entity work.v
    port map (mbare => lv);
  ekjle : entity work.zsmlmnrikz
    port map (pa => lv, fzq => clra);
  vkwulkmg : entity work.v
    port map (mbare => zn);
  bcsnpbg : entity work.zsmlmnrikz
    port map (pa => lv, fzq => clra);
end w;



-- Seed after: 5985720142212329322,6329330932550885447
