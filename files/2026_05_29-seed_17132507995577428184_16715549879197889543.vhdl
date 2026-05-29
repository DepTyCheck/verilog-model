-- Seed: 17132507995577428184,16715549879197889543

library ieee;
use ieee.std_logic_1164.all;

entity mguo is
  port (jgaxa : inout std_logic; qbgygrz : in real; k : in integer_vector(4 downto 4));
end mguo;



architecture oe of mguo is
  
begin
  
end oe;



entity fxnotqtmvv is
  port (kylwwtjgo : in integer);
end fxnotqtmvv;

library ieee;
use ieee.std_logic_1164.all;

architecture yclkfokcg of fxnotqtmvv is
  signal wtxqqcic : std_logic;
  signal prfuuvv : real;
  signal rdf : real;
  signal g : std_logic;
  signal afhlyxqcj : integer_vector(4 downto 4);
  signal r : real;
  signal gqqeln : std_logic;
begin
  i : entity work.mguo
    port map (jgaxa => gqqeln, qbgygrz => r, k => afhlyxqcj);
  uyycqst : entity work.mguo
    port map (jgaxa => g, qbgygrz => rdf, k => afhlyxqcj);
  po : entity work.mguo
    port map (jgaxa => gqqeln, qbgygrz => prfuuvv, k => afhlyxqcj);
  oldxot : entity work.mguo
    port map (jgaxa => wtxqqcic, qbgygrz => rdf, k => afhlyxqcj);
end yclkfokcg;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (fwaoialdc : linkage boolean_vector(1 to 3); xqd : out std_logic);
end o;



architecture llsjvnmfi of o is
  signal zifcjezfn : integer;
  signal njb : integer_vector(4 downto 4);
  signal fxaqtjtvyw : real;
begin
  anm : entity work.mguo
    port map (jgaxa => xqd, qbgygrz => fxaqtjtvyw, k => njb);
  aar : entity work.fxnotqtmvv
    port map (kylwwtjgo => zifcjezfn);
  sygnn : entity work.fxnotqtmvv
    port map (kylwwtjgo => zifcjezfn);
end llsjvnmfi;



-- Seed after: 102290619508884673,16715549879197889543
