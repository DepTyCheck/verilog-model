-- Seed: 6034309833414353313,11060400121348610183



entity zawofdb is
  port (kobwe : out real; cwthwuq : inout time; xyz : linkage severity_level; uhbzmup : in character);
end zawofdb;



architecture x of zawofdb is
  
begin
  
end x;



entity pel is
  port (ljtt : inout integer; wd : linkage real; xiyaqjxaq : linkage character; kybrsajtv : buffer character);
end pel;



architecture gjkv of pel is
  signal lbvui : severity_level;
  signal chw : time;
  signal dkwuwud : real;
begin
  s : entity work.zawofdb
    port map (kobwe => dkwuwud, cwthwuq => chw, xyz => lbvui, uhbzmup => kybrsajtv);
end gjkv;



entity ygpylwjxar is
  port (vra : out integer; e : inout integer; bgzucuyvx : buffer bit; shrwzy : buffer severity_level);
end ygpylwjxar;



architecture ax of ygpylwjxar is
  signal tgxvv : character;
  signal omygugenpv : character;
  signal d : real;
begin
  xktfija : entity work.pel
    port map (ljtt => e, wd => d, xiyaqjxaq => omygugenpv, kybrsajtv => tgxvv);
end ax;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (plopxydoh : in std_logic; iznse : out real; zceqjqt : linkage std_logic; llh : out real);
end o;



architecture azjopi of o is
  signal ixsbnts : character;
  signal fxuomgdl : character;
  signal rz : integer;
  signal dmrcfbkrke : character;
  signal pldikyjmf : severity_level;
  signal hiomelbzo : time;
begin
  h : entity work.zawofdb
    port map (kobwe => llh, cwthwuq => hiomelbzo, xyz => pldikyjmf, uhbzmup => dmrcfbkrke);
  zrnafcfrc : entity work.pel
    port map (ljtt => rz, wd => llh, xiyaqjxaq => fxuomgdl, kybrsajtv => ixsbnts);
end azjopi;



-- Seed after: 2956635273340198537,11060400121348610183
