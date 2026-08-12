-- Seed: 5843289312134204760,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity gws is
  port (y : buffer boolean; dy : out std_logic_vector(4 to 0); sco : inout real_vector(1 to 1); dk : out integer);
end gws;

architecture pxs of gws is
  
begin
  -- Single-driven assignments
  y <= TRUE;
  
  -- Multi-driven assignments
  dy <= "";
  dy <= dy;
  dy <= (others => '0');
end pxs;

library ieee;
use ieee.std_logic_1164.all;

entity fwxpvub is
  port (mu : linkage std_logic; bshyfwnr : buffer std_logic_vector(4 downto 3); sk : in integer_vector(1 downto 0));
end fwxpvub;

library ieee;
use ieee.std_logic_1164.all;

architecture zwkljmlxyu of fwxpvub is
  signal ue : integer;
  signal ahnsjgvh : real_vector(1 to 1);
  signal afsavnfmm : boolean;
  signal r : integer;
  signal hjauuh : real_vector(1 to 1);
  signal snls : boolean;
  signal ypuyddp : integer;
  signal bsixjpogp : real_vector(1 to 1);
  signal nrmqzpq : std_logic_vector(4 to 0);
  signal wrjl : boolean;
begin
  cgso : entity work.gws
    port map (y => wrjl, dy => nrmqzpq, sco => bsixjpogp, dk => ypuyddp);
  a : entity work.gws
    port map (y => snls, dy => nrmqzpq, sco => hjauuh, dk => r);
  vmavbj : entity work.gws
    port map (y => afsavnfmm, dy => nrmqzpq, sco => ahnsjgvh, dk => ue);
  
  -- Multi-driven assignments
  bshyfwnr <= bshyfwnr;
  bshyfwnr <= bshyfwnr;
end zwkljmlxyu;

entity ek is
  port (xfaxy : buffer integer; wjr : inout boolean);
end ek;

library ieee;
use ieee.std_logic_1164.all;

architecture nz of ek is
  signal cwocoqtdio : integer_vector(1 downto 0);
  signal urbfazjx : std_logic_vector(4 downto 3);
  signal j : std_logic;
  signal ylu : real_vector(1 to 1);
  signal slluc : std_logic_vector(4 to 0);
  signal o : boolean;
begin
  jffgcgccb : entity work.gws
    port map (y => o, dy => slluc, sco => ylu, dk => xfaxy);
  mhjfgumret : entity work.fwxpvub
    port map (mu => j, bshyfwnr => urbfazjx, sk => cwocoqtdio);
  
  -- Single-driven assignments
  wjr <= TRUE;
  cwocoqtdio <= (2#1_1#, 01414);
  
  -- Multi-driven assignments
  urbfazjx <= urbfazjx;
  slluc <= "";
  slluc <= slluc;
end nz;

entity xte is
  port (xbdiofrpmi : buffer real_vector(1 to 2); fulxtebf : buffer integer; armckphiy : inout integer);
end xte;

library ieee;
use ieee.std_logic_1164.all;

architecture qvhwzbee of xte is
  signal noe : integer;
  signal hcyjos : real_vector(1 to 1);
  signal lsbrvalspi : boolean;
  signal xtdilw : real_vector(1 to 1);
  signal nz : boolean;
  signal necaadhlf : real_vector(1 to 1);
  signal enccgalnu : std_logic_vector(4 to 0);
  signal pldeqhxahj : boolean;
begin
  b : entity work.gws
    port map (y => pldeqhxahj, dy => enccgalnu, sco => necaadhlf, dk => armckphiy);
  xrbd : entity work.gws
    port map (y => nz, dy => enccgalnu, sco => xtdilw, dk => fulxtebf);
  awbdjjk : entity work.gws
    port map (y => lsbrvalspi, dy => enccgalnu, sco => hcyjos, dk => noe);
  
  -- Single-driven assignments
  xbdiofrpmi <= (4.4_1, 8#3_4_6_5.0#);
  
  -- Multi-driven assignments
  enccgalnu <= (others => '0');
  enccgalnu <= enccgalnu;
end qvhwzbee;



-- Seed after: 12074060957482646118,8412319452373742525
