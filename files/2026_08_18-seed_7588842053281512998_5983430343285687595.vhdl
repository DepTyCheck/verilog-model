-- Seed: 7588842053281512998,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity viji is
  port (idkgdrgg : out time; qvqgdpkec : inout std_logic);
end viji;

architecture xbfmwou of viji is
  
begin
  -- Single-driven assignments
  idkgdrgg <= idkgdrgg;
  
  -- Multi-driven assignments
  qvqgdpkec <= 'H';
end xbfmwou;

entity ny is
  port (dktkx : in time);
end ny;

library ieee;
use ieee.std_logic_1164.all;

architecture yw of ny is
  signal ihcro : std_logic;
  signal bllq : time;
  signal xjba : time;
  signal vkqjflt : std_logic;
  signal qeofqwfe : time;
  signal fibitcfpk : std_logic;
  signal c : time;
begin
  rqqf : entity work.viji
    port map (idkgdrgg => c, qvqgdpkec => fibitcfpk);
  ho : entity work.viji
    port map (idkgdrgg => qeofqwfe, qvqgdpkec => vkqjflt);
  iveehdz : entity work.viji
    port map (idkgdrgg => xjba, qvqgdpkec => fibitcfpk);
  fuahyl : entity work.viji
    port map (idkgdrgg => bllq, qvqgdpkec => ihcro);
  
  -- Multi-driven assignments
  fibitcfpk <= fibitcfpk;
end yw;

entity bo is
  port (dgo : in integer; vfethuqffq : buffer bit);
end bo;

library ieee;
use ieee.std_logic_1164.all;

architecture zzvxj of bo is
  signal afqsqmwez : std_logic;
  signal lzshlsex : time;
  signal ulpwii : std_logic;
  signal dcoyazb : time;
begin
  wupo : entity work.viji
    port map (idkgdrgg => dcoyazb, qvqgdpkec => ulpwii);
  m : entity work.viji
    port map (idkgdrgg => lzshlsex, qvqgdpkec => afqsqmwez);
  twgsapi : entity work.ny
    port map (dktkx => dcoyazb);
  
  -- Single-driven assignments
  vfethuqffq <= '0';
  
  -- Multi-driven assignments
  ulpwii <= 'W';
  ulpwii <= ulpwii;
  ulpwii <= 'H';
  afqsqmwez <= 'X';
end zzvxj;

entity riwiaogiv is
  port (ex : buffer boolean);
end riwiaogiv;

library ieee;
use ieee.std_logic_1164.all;

architecture vg of riwiaogiv is
  signal o : std_logic;
  signal p : time;
  signal supggv : bit;
  signal shwc : integer;
  signal ouc : time;
begin
  offn : entity work.ny
    port map (dktkx => ouc);
  tmuv : entity work.bo
    port map (dgo => shwc, vfethuqffq => supggv);
  irr : entity work.viji
    port map (idkgdrgg => p, qvqgdpkec => o);
  pjlpnauwp : entity work.viji
    port map (idkgdrgg => ouc, qvqgdpkec => o);
  
  -- Single-driven assignments
  ex <= FALSE;
  shwc <= shwc;
end vg;



-- Seed after: 13122954566379640703,5983430343285687595
