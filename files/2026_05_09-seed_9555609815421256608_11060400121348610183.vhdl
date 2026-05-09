-- Seed: 9555609815421256608,11060400121348610183



entity iap is
  port (qwaz : in severity_level; pzdks : inout integer; j : out real; gv : out real);
end iap;



architecture c of iap is
  
begin
  
end c;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (wrwfxpkm : linkage std_logic; edmbxzlj : inout integer; kv : inout integer);
end n;



architecture ubcrcvtkhw of n is
  signal jvxgnr : real;
  signal dlzvnfui : real;
  signal iyco : integer;
  signal jyis : severity_level;
  signal awjgkqdnh : real;
  signal cgxss : real;
  signal yrxkmxegz : integer;
  signal bz : severity_level;
  signal gv : real;
  signal zhyjjsk : real;
  signal gs : integer;
  signal akfrb : real;
  signal v : real;
  signal ruy : severity_level;
begin
  ukgwp : entity work.iap
    port map (qwaz => ruy, pzdks => kv, j => v, gv => akfrb);
  ydupzvceu : entity work.iap
    port map (qwaz => ruy, pzdks => gs, j => zhyjjsk, gv => gv);
  kh : entity work.iap
    port map (qwaz => bz, pzdks => yrxkmxegz, j => cgxss, gv => awjgkqdnh);
  b : entity work.iap
    port map (qwaz => jyis, pzdks => iyco, j => dlzvnfui, gv => jvxgnr);
end ubcrcvtkhw;

library ieee;
use ieee.std_logic_1164.all;

entity nbbyafn is
  port (yt : in time; oegzr : in time; k : out std_logic);
end nbbyafn;

library ieee;
use ieee.std_logic_1164.all;

architecture bcd of nbbyafn is
  signal nlyzlqu : real;
  signal itkixqrs : real;
  signal zutunog : integer;
  signal opclfaxxvx : real;
  signal rwpjy : real;
  signal b : integer;
  signal dvab : severity_level;
  signal lc : integer;
  signal syjrvqbwqh : integer;
  signal lklasrovv : std_logic;
  signal f : integer;
  signal skvansz : integer;
begin
  hcpixvpwx : entity work.n
    port map (wrwfxpkm => k, edmbxzlj => skvansz, kv => f);
  huyu : entity work.n
    port map (wrwfxpkm => lklasrovv, edmbxzlj => syjrvqbwqh, kv => lc);
  u : entity work.iap
    port map (qwaz => dvab, pzdks => b, j => rwpjy, gv => opclfaxxvx);
  fwzaulxdbs : entity work.iap
    port map (qwaz => dvab, pzdks => zutunog, j => itkixqrs, gv => nlyzlqu);
end bcd;

library ieee;
use ieee.std_logic_1164.all;

entity mx is
  port (mknfi : out std_logic; sozcxyenbs : out severity_level);
end mx;



architecture gnepgkghp of mx is
  signal uspbpz : real;
  signal bbmr : real;
  signal hyu : integer;
  signal y : time;
  signal sbjrlf : time;
  signal ncpmdj : real;
  signal aanntz : real;
  signal qecvasds : integer;
  signal vtjheml : severity_level;
  signal jicxi : real;
  signal kpvug : real;
  signal knjxkbrg : integer;
  signal isom : severity_level;
begin
  fma : entity work.iap
    port map (qwaz => isom, pzdks => knjxkbrg, j => kpvug, gv => jicxi);
  jz : entity work.iap
    port map (qwaz => vtjheml, pzdks => qecvasds, j => aanntz, gv => ncpmdj);
  csybn : entity work.nbbyafn
    port map (yt => sbjrlf, oegzr => y, k => mknfi);
  ghg : entity work.iap
    port map (qwaz => sozcxyenbs, pzdks => hyu, j => bbmr, gv => uspbpz);
end gnepgkghp;



-- Seed after: 2467417198139687936,11060400121348610183
