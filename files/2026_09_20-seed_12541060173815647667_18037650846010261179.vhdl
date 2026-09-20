-- Seed: 12541060173815647667,18037650846010261179

entity wsqtdjmi is
  port (hp : inout real);
end wsqtdjmi;

architecture ijcpgqhu of wsqtdjmi is
  
begin
  -- Single-driven assignments
  hp <= hp;
end ijcpgqhu;

library ieee;
use ieee.std_logic_1164.all;

entity skq is
  port (dkryhdfthj : inout std_logic);
end skq;

architecture bffqhwjf of skq is
  signal mcobsegdso : real;
begin
  qnuqu : entity work.wsqtdjmi
    port map (hp => mcobsegdso);
end bffqhwjf;

library ieee;
use ieee.std_logic_1164.all;

entity fbyrucvnf is
  port (g : inout std_logic_vector(2 downto 0); x : in bit_vector(4 to 4); xclm : in boolean);
end fbyrucvnf;

architecture bjftfape of fbyrucvnf is
  signal dnfiedlb : real;
  signal aviuyudk : real;
  signal gyqhpne : real;
begin
  wulp : entity work.wsqtdjmi
    port map (hp => gyqhpne);
  ozsttwgi : entity work.wsqtdjmi
    port map (hp => aviuyudk);
  jldsx : entity work.wsqtdjmi
    port map (hp => dnfiedlb);
  
  -- Multi-driven assignments
  g <= g;
end bjftfape;

library ieee;
use ieee.std_logic_1164.all;

entity xnlelzfiej is
  port (pt : in time; rgvibzlu : buffer std_logic);
end xnlelzfiej;

library ieee;
use ieee.std_logic_1164.all;

architecture ngunbig of xnlelzfiej is
  signal w : boolean;
  signal gxspqlxirc : bit_vector(4 to 4);
  signal cuggewcebb : std_logic_vector(2 downto 0);
  signal sdejrj : real;
  signal vddsyr : real;
begin
  zuobpgymg : entity work.wsqtdjmi
    port map (hp => vddsyr);
  qupzzawpy : entity work.wsqtdjmi
    port map (hp => sdejrj);
  jbvkq : entity work.fbyrucvnf
    port map (g => cuggewcebb, x => gxspqlxirc, xclm => w);
  boxhwm : entity work.skq
    port map (dkryhdfthj => rgvibzlu);
  
  -- Single-driven assignments
  w <= TRUE;
  gxspqlxirc <= (others => '0');
  
  -- Multi-driven assignments
  rgvibzlu <= 'Z';
  rgvibzlu <= rgvibzlu;
end ngunbig;



-- Seed after: 12258334936040464444,18037650846010261179
