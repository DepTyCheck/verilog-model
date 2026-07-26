-- Seed: 13110008591001669672,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity wdp is
  port (mejsjk : buffer std_logic_vector(0 to 1); pptxyaog : buffer std_logic_vector(2 to 4); mvimdvk : buffer std_logic_vector(3 downto 2));
end wdp;

architecture gon of wdp is
  
begin
  -- Multi-driven assignments
  mvimdvk <= mejsjk;
end gon;

entity c is
  port (zgwcjoodic : inout integer; uqr : inout time; smgsjmi : buffer time);
end c;

architecture opfccnfq of c is
  
begin
  -- Single-driven assignments
  zgwcjoodic <= zgwcjoodic;
  uqr <= smgsjmi;
end opfccnfq;

library ieee;
use ieee.std_logic_1164.all;

entity usobzoufl is
  port (k : out std_logic_vector(0 to 4); tzlyyjuweg : linkage integer; d : linkage integer);
end usobzoufl;

library ieee;
use ieee.std_logic_1164.all;

architecture kpupxyowk of usobzoufl is
  signal lj : time;
  signal mthcssrkgu : time;
  signal ofuo : integer;
  signal ebkganbtce : std_logic_vector(3 downto 2);
  signal xy : std_logic_vector(0 to 1);
  signal xdbsugh : std_logic_vector(0 to 1);
  signal lax : std_logic_vector(2 to 4);
  signal falmx : std_logic_vector(3 downto 2);
begin
  wmjiexik : entity work.wdp
    port map (mejsjk => falmx, pptxyaog => lax, mvimdvk => falmx);
  kriojshj : entity work.wdp
    port map (mejsjk => xdbsugh, pptxyaog => lax, mvimdvk => falmx);
  yplksge : entity work.wdp
    port map (mejsjk => xy, pptxyaog => lax, mvimdvk => ebkganbtce);
  klvirt : entity work.c
    port map (zgwcjoodic => ofuo, uqr => mthcssrkgu, smgsjmi => lj);
  
  -- Multi-driven assignments
  falmx <= "HW";
  k <= "1XU1Z";
end kpupxyowk;

entity eqchv is
  port (wlyswj : in real);
end eqchv;

library ieee;
use ieee.std_logic_1164.all;

architecture lzfecb of eqchv is
  signal kaxkgwyek : integer;
  signal bwoivgwgch : integer;
  signal btljej : std_logic_vector(0 to 4);
  signal rydrhlw : std_logic_vector(2 to 4);
  signal ay : std_logic_vector(3 downto 2);
begin
  kvdbapopt : entity work.wdp
    port map (mejsjk => ay, pptxyaog => rydrhlw, mvimdvk => ay);
  pxj : entity work.wdp
    port map (mejsjk => ay, pptxyaog => rydrhlw, mvimdvk => ay);
  vvnaaghimh : entity work.usobzoufl
    port map (k => btljej, tzlyyjuweg => bwoivgwgch, d => kaxkgwyek);
  
  -- Multi-driven assignments
  ay <= ay;
  ay <= ay;
  ay <= ('U', 'H');
end lzfecb;



-- Seed after: 4637814314124466511,7808623373429384027
