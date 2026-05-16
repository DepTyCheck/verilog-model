-- Seed: 11702548712193665652,11218062946904572163



entity teyw is
  port (x : inout time);
end teyw;



architecture uuej of teyw is
  
begin
  
end uuej;



entity lxqda is
  port (ghcktucn : in integer);
end lxqda;



architecture gpjcwom of lxqda is
  signal sfwcumfm : time;
begin
  rwafdjj : entity work.teyw
    port map (x => sfwcumfm);
end gpjcwom;

library ieee;
use ieee.std_logic_1164.all;

entity hgdyfr is
  port (sihr : in character; rutehz : inout std_logic; udhz : inout time; eegrrb : out integer);
end hgdyfr;



architecture imoaylixz of hgdyfr is
  signal tjbw : time;
  signal ac : time;
begin
  s : entity work.lxqda
    port map (ghcktucn => eegrrb);
  aao : entity work.teyw
    port map (x => ac);
  foztjcnvx : entity work.teyw
    port map (x => tjbw);
  bppv : entity work.teyw
    port map (x => udhz);
end imoaylixz;



entity tslhaat is
  port (uau : buffer time; odiiplum : buffer integer);
end tslhaat;

library ieee;
use ieee.std_logic_1164.all;

architecture rhwwppmwh of tslhaat is
  signal duq : std_logic;
  signal cnhswiawfp : time;
  signal pvarn : integer;
  signal qlleuwjrs : time;
  signal tprlzocp : std_logic;
  signal elng : character;
begin
  w : entity work.hgdyfr
    port map (sihr => elng, rutehz => tprlzocp, udhz => qlleuwjrs, eegrrb => pvarn);
  sayt : entity work.teyw
    port map (x => cnhswiawfp);
  zgyyon : entity work.lxqda
    port map (ghcktucn => pvarn);
  ayud : entity work.hgdyfr
    port map (sihr => elng, rutehz => duq, udhz => uau, eegrrb => odiiplum);
end rhwwppmwh;



-- Seed after: 4771330128792962151,11218062946904572163
