-- Seed: 11685836893402342395,6298042963991371649

library ieee;
use ieee.std_logic_1164.all;

entity neqh is
  port (qfrxmf : linkage std_logic_vector(1 to 4); apfcpnh : out integer; xw : inout time; vsl : linkage time);
end neqh;



architecture erxqv of neqh is
  
begin
  
end erxqv;



entity uc is
  port (ydivjd : buffer integer);
end uc;

library ieee;
use ieee.std_logic_1164.all;

architecture jxbxyln of uc is
  signal qhwcokbed : time;
  signal cgyfr : integer;
  signal hsyh : time;
  signal iusyzpw : time;
  signal equdw : integer;
  signal nzbg : std_logic_vector(1 to 4);
begin
  vtbpuzulx : entity work.neqh
    port map (qfrxmf => nzbg, apfcpnh => equdw, xw => iusyzpw, vsl => hsyh);
  knsvmf : entity work.neqh
    port map (qfrxmf => nzbg, apfcpnh => cgyfr, xw => qhwcokbed, vsl => iusyzpw);
  e : entity work.neqh
    port map (qfrxmf => nzbg, apfcpnh => ydivjd, xw => hsyh, vsl => qhwcokbed);
end jxbxyln;



-- Seed after: 14316669557865786182,6298042963991371649
