-- Seed: 7817553984877129596,2983771601630957889

use std.reflection.all;

entity i is
  port (qxrejxqmkr : in boolean; cikf : buffer boolean_vector(2 downto 2); variable tu : inout subtype_mirror_pt; g : buffer integer);
end i;

architecture c of i is
  
begin
  
end c;

use std.reflection.all;

entity wbq is
  port (variable pklkzizl : inout enumeration_value_mirror_pt; qfc : out time);
end wbq;

use std.reflection.all;

architecture jukk of wbq is
  signal bglsuypcp : integer;
  shared variable mqylimm : subtype_mirror_pt;
  signal lxex : boolean_vector(2 downto 2);
  signal zi : integer;
  shared variable vjvx : subtype_mirror_pt;
  signal flprrth : boolean_vector(2 downto 2);
  signal d : boolean;
  signal vzgcankw : integer;
  shared variable x : subtype_mirror_pt;
  signal knwsvj : boolean_vector(2 downto 2);
  signal xnappdwpds : integer;
  shared variable rnzntsur : subtype_mirror_pt;
  signal wvvbyhtxz : boolean_vector(2 downto 2);
  signal bqnzipf : boolean;
begin
  ixxqw : entity work.i
    port map (qxrejxqmkr => bqnzipf, cikf => wvvbyhtxz, tu => rnzntsur, g => xnappdwpds);
  ecbf : entity work.i
    port map (qxrejxqmkr => bqnzipf, cikf => knwsvj, tu => x, g => vzgcankw);
  r : entity work.i
    port map (qxrejxqmkr => d, cikf => flprrth, tu => vjvx, g => zi);
  kn : entity work.i
    port map (qxrejxqmkr => d, cikf => lxex, tu => mqylimm, g => bglsuypcp);
  
  -- Single-driven assignments
  qfc <= qfc;
  bqnzipf <= FALSE;
end jukk;



-- Seed after: 5749872129559231450,2983771601630957889
