-- Seed: 11673221022713817275,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity uotef is
  port (sx : in std_logic; zqmcptya : out std_logic_vector(0 downto 2); tzclal : linkage bit; eqfynrvce : linkage time);
end uotef;

architecture cuyzrjew of uotef is
  
begin
  -- Multi-driven assignments
  zqmcptya <= zqmcptya;
  zqmcptya <= (others => '0');
end cuyzrjew;

entity tgsfriy is
  port (ok : in time_vector(3 to 1));
end tgsfriy;

library ieee;
use ieee.std_logic_1164.all;

architecture gp of tgsfriy is
  signal xiqzssu : time;
  signal q : bit;
  signal iusigqsqq : time;
  signal cwxmbdgk : bit;
  signal tk : std_logic_vector(0 downto 2);
  signal ztx : time;
  signal dmwx : bit;
  signal wtgi : std_logic_vector(0 downto 2);
  signal urh : std_logic;
begin
  bvlhqamhlk : entity work.uotef
    port map (sx => urh, zqmcptya => wtgi, tzclal => dmwx, eqfynrvce => ztx);
  txtcj : entity work.uotef
    port map (sx => urh, zqmcptya => tk, tzclal => cwxmbdgk, eqfynrvce => iusigqsqq);
  u : entity work.uotef
    port map (sx => urh, zqmcptya => wtgi, tzclal => q, eqfynrvce => xiqzssu);
end gp;

entity ghwdlom is
  port (njeich : in bit_vector(0 downto 3); eki : linkage bit_vector(0 to 3); yfhzdzhk : inout bit_vector(0 downto 1));
end ghwdlom;

library ieee;
use ieee.std_logic_1164.all;

architecture xmiyszbey of ghwdlom is
  signal ccnfcjhvpy : time;
  signal ymdbyoznqv : bit;
  signal ryqzluo : std_logic_vector(0 downto 2);
  signal lbf : std_logic;
  signal mcrrj : time;
  signal jwphhor : bit;
  signal azkqxx : std_logic_vector(0 downto 2);
  signal v : time;
  signal dnvg : bit;
  signal qpdurv : std_logic_vector(0 downto 2);
  signal fzncm : std_logic;
begin
  jiqble : entity work.uotef
    port map (sx => fzncm, zqmcptya => qpdurv, tzclal => dnvg, eqfynrvce => v);
  p : entity work.uotef
    port map (sx => fzncm, zqmcptya => azkqxx, tzclal => jwphhor, eqfynrvce => mcrrj);
  vbyetrbhzw : entity work.uotef
    port map (sx => lbf, zqmcptya => ryqzluo, tzclal => ymdbyoznqv, eqfynrvce => ccnfcjhvpy);
  
  -- Single-driven assignments
  yfhzdzhk <= yfhzdzhk;
  
  -- Multi-driven assignments
  fzncm <= fzncm;
end xmiyszbey;

entity horhlkw is
  port (xyr : buffer character; emtfyespx : buffer integer_vector(3 to 1); ospwzgiby : linkage time);
end horhlkw;

library ieee;
use ieee.std_logic_1164.all;

architecture aoltcckh of horhlkw is
  signal xr : bit_vector(0 downto 1);
  signal idkeabf : bit_vector(0 to 3);
  signal pmd : bit_vector(0 downto 3);
  signal mnvvo : bit;
  signal bfsf : std_logic_vector(0 downto 2);
  signal rju : std_logic;
begin
  raehu : entity work.uotef
    port map (sx => rju, zqmcptya => bfsf, tzclal => mnvvo, eqfynrvce => ospwzgiby);
  alcty : entity work.ghwdlom
    port map (njeich => pmd, eki => idkeabf, yfhzdzhk => xr);
  
  -- Single-driven assignments
  pmd <= pmd;
  xyr <= 'g';
  emtfyespx <= emtfyespx;
  
  -- Multi-driven assignments
  rju <= 'X';
  rju <= '0';
  rju <= 'W';
end aoltcckh;



-- Seed after: 2039397352370212781,10594830431004325987
