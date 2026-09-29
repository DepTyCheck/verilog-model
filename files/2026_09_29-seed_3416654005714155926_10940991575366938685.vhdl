-- Seed: 3416654005714155926,10940991575366938685

entity mfflnicd is
  port (e : out real);
end mfflnicd;

architecture pxjdscqbj of mfflnicd is
  
begin
  -- Single-driven assignments
  e <= 44.4;
end pxjdscqbj;

library ieee;
use ieee.std_logic_1164.all;

entity tapslwn is
  port (ppjurzfdy : buffer std_logic; ximxqz : linkage std_logic; y : inout std_logic; nppzypahij : buffer std_logic_vector(3 to 2));
end tapslwn;

architecture r of tapslwn is
  signal fn : real;
begin
  nyajvsixws : entity work.mfflnicd
    port map (e => fn);
end r;

entity silywpek is
  port (byylbyog : inout boolean);
end silywpek;

library ieee;
use ieee.std_logic_1164.all;

architecture ssdpj of silywpek is
  signal vtuyrrde : real;
  signal tlzfs : real;
  signal vhhxcaood : real;
  signal aspmwnh : std_logic_vector(3 to 2);
  signal fdi : std_logic;
  signal jubgp : std_logic;
  signal ryydfes : std_logic;
begin
  cira : entity work.tapslwn
    port map (ppjurzfdy => ryydfes, ximxqz => jubgp, y => fdi, nppzypahij => aspmwnh);
  x : entity work.mfflnicd
    port map (e => vhhxcaood);
  d : entity work.mfflnicd
    port map (e => tlzfs);
  txs : entity work.mfflnicd
    port map (e => vtuyrrde);
  
  -- Single-driven assignments
  byylbyog <= FALSE;
  
  -- Multi-driven assignments
  ryydfes <= 'W';
  fdi <= ryydfes;
end ssdpj;



-- Seed after: 555130694598773748,10940991575366938685
